%%%----------------------------------------------------------------------
%%% File    : fdoc.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Heuristic sourcecode documentation extractor
%%%           (A poor man's 'edoc')
%%% Created : 26 Feb 2003 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(fdoc).
-author('luke@bluetail.com').

-import(lists,
	[reverse/1, splitwith/2, takewhile/2, flatten/1, map/2,
	 foreach/2, member/2, sort/1, any/2]).

%% Server-based interface
-export([apropos/1, get_apropos/1,
	 describe/1, describe/2, describe/3,
	 description/1, description/2, description/3,
	 stop/0]).

%% Function-based interface
-export([describe_file/1, file/1, string/1]).
-export([describe2/1, describe2/2, describe2/3]).

%% Internal server exports
-export([init/0,loop/0]).

%% ----------------------------------------------------------------------
%% Server half. Calling these functions causes a server to be created,
%% which scans the sources for all loaded modules. You can use stop()
%% to shut it down, so that the next request will rebuild the database.
%% ----------------------------------------------------------------------

%% Interface

%% Print apropos information by regexp.
apropos(Regexp) ->
    case get_apropos(Regexp) of
	{ok, Matches} ->
	    print_matches(Matches);
	Err ->
	    Err
    end.

get_apropos(Regexp) ->
    ensure_started(),
    case regexp:parse(Regexp) of
	{ok, RE} ->
	    fdoc ! {apropos, self(), RE},
	    receive
		{apropos, Matches} ->
		    {ok, Matches}
	    end;
	Err ->
	    Err
    end.

%% Print a description of all functions matching Module, Function, and
%% Arity. Function and module arguments are optional.

describe(M) ->
    describe(M, '_', '_').
describe(M, F) ->
    describe(M, F, '_').
describe(M, F, A) ->
  {ok, Matches} = description(M, F, A),
  print_matches(Matches).

description(M) ->
    description(M, '_', '_').

description(M, F) ->
    description(M, F, '_').

description(M, F, A) ->
    ensure_started(),
    fdoc ! {describe, self(), M, F, A},
    receive
	{describe, Matches} -> {ok, Matches}
    end.

%% This function does not use the cache
describe2(M) ->
    describe2(M, '_', '_').
describe2(M, F) ->
    describe2(M, F, '_').
describe2(M, F, A) ->
    case distel:find_source(M) of
	{ok, Sourcefile} ->
	    case file(Sourcefile) of
		{ok, Docs} ->
		    {ok, lists:zf(fun({Fn, Arity, DocStr}) when F=='_'; Fn==F ->
					  if A == '_'; A == Arity ->
						  {true, {M,Fn, Arity, DocStr}};
					     true ->
						  false
					  end;
				     (_) ->
					  false
				  end, Docs)};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.


%% Stop the fdoc server. You can use this to flush the database.
stop() ->
    catch (fdoc ! {stop, self()}),
    receive {stop, ok} -> ok end.

%% Internals

print_matches(Ms) ->
    foreach(fun({Mod, Fn, Arity, Doc}) ->
		    io:format("~p:~p/~p:~n~s~n",
			      [Mod, Fn, Arity, indent(Doc, "  ")])
	    end,
	    sort(Ms)).

%% Ensure that the 'fdoc' server has started.
ensure_started() ->
    case whereis(?MODULE) of
	undefined ->
	    Pid = proc_lib:spawn(?MODULE, init, []),
	    register(?MODULE, Pid),
	    {ok, Pid};
	Pid ->
	    {ok, Pid}
    end.

init() ->
    init_db(),
    loop().

init_db() ->
    ets:new(?MODULE, [public, bag, named_table]),
    foreach(fun import_module/1, [M || {M, _Objfile} <- code:all_loaded()]).

loop() ->
    receive
	{stop, From} ->
	    From ! {stop, ok},
	    ok;
	{apropos, From, RE} ->
	    Matches = [{M,F,A,D} || {M,F,A,D} <- ets:tab2list(?MODULE),
				    any(fun(S) ->
						regexp:match(S, RE) /= nomatch
					end,
					[D,
					 atom_to_list(M),
					 atom_to_list(F)])],
	    From ! {apropos, Matches},
	    ?MODULE:loop();
	{describe, From, M, F, A} ->
	    case code:is_loaded(M) of
		false ->
		    %% Have to reload the database first..
		    code:ensure_loaded(M),
		    ets:delete(?MODULE),
		    init_db();
		_ ->
		    ok
	    end,
	    From ! {describe, ets:match_object(?MODULE, {M, F, A, '_'})},
	    ?MODULE:loop()
    end.

import_module(Mod) ->
    case distel:find_source(Mod) of
	{ok, Sourcefile} ->
	    case file(Sourcefile) of
		{ok, Docs} ->
		    foreach(fun({Fn, Arity, DocStr}) ->
				    ets:insert(?MODULE,{Mod,Fn,Arity,DocStr})
			    end,
			    just_exports(Mod, Docs));
		{error, _Reason} ->
		    skipped
	    end;
	{error, _Reason} ->
	    skipped
    end.

just_exports(Mod, Docs) ->
    [{F,A,D} || {F,A,D} <- Docs,
		member({F, A}, Mod:module_info(exports))].

%% ----------------------------------------------------------------------
%% Function half, for extracting documentation from files and strings.
%% ----------------------------------------------------------------------

%% Interface

%% Print a description of all the documented exported functions in a
%% file.
describe_file(Name) ->
    case file(Name) of
	{ok, Docs} ->
	    foreach(fun({Fn, Arity, Doc}) ->
			    io:format("~p/~p:~n~s~n",
				      [Fn, Arity, indent(Doc, "  ")])
		    end,
		    Docs);
	{error, Rsn} ->
	    {error, Rsn}
    end.

%% Return a description of all documented exported functions in
%% a file.
%% Returns: {Function, Arity, Documentation}
file(Name) ->
    case file:read_file(Name) of
	{ok, Bin} ->
	    {ok, string(binary_to_list(Bin))};
	{error, Reason} ->
	    {error, Reason}
    end.

%% Return a description of all documented exported functions in
%% a string.
%% Returns: {Function, Arity, Documentation}
string(S) ->
    remove_dups(scan_lines(S, [])).

%% Internals

%% If we have multiple definitions of the same function/arity, just
%% keep the first one.
remove_dups([{F,A,Doc}, {F,A,_} | T]) ->
    remove_dups([{F,A,Doc}|T]);
remove_dups([H|T]) ->
    [H|remove_dups(T)];
remove_dups([]) ->
    [].

scan_lines([], _) ->
    [];
scan_lines(S0, Acc) when hd(S0) == $% ->
    {CommentLine, S1} = take_line(S0),
    scan_lines(S1, [CommentLine|Acc]);
scan_lines("\n"++S, _Acc) when hd(S) == $% ->
    %% Blank followed by a new comment: flush old comment
    scan_lines(S, []);
scan_lines("\n"++S, Acc) ->
    %% Blank not followed by a comment: keep the current comment
    scan_lines(S, Acc);
scan_lines(S0, Acc) ->
    case function_start_char(hd(S0)) of
	true ->
	    scan_function(S0, comments_to_doc(reverse(Acc)));
	false ->
	    scan_lines(drop_line(S0), [])
    end.

scan_function(S0, Comments) ->
    case take_function_head(S0) of
	{ok, Head, S1} ->
	    S2 = drop_line(S1),		% skip to next fresh line
	    case parse_function_head(Head) of
		{ok, Function, Arity} ->
		    if Comments == [] ->
			    scan_lines(S2, []);
		       true ->
			    [{Function, Arity, Comments} | scan_lines(S2, [])]
		    end;
		error ->
		    scan_lines(S2, [])
	    end;
	eos ->
	    []
    end.

function_start_char(Ch) when Ch >= $a, Ch =< $z -> true;
function_start_char(Ch) when Ch >= $A, Ch =< $Z -> true;
function_start_char($')                         -> true;
function_start_char(_)                          -> false.

%% Parse a function head into name and arity.
%% Returns: {ok, Name, Arity} | error
parse_function_head(S0) ->
    %% Poor man's approach: rewind back to the closing ')', tack on a
    %% '.', and see if we can parse it as a function call.
    S1 = takewhile(fun(Ch) -> Ch /= $) end, S0) ++ ").",
    case erl_scan:string(S1) of
	{ok, Scan, _} ->
	    case erl_parse:parse_exprs(Scan) of
		{ok, [{call,_,{atom,_,Fname},Args}]} ->
		    {ok, Fname, length(Args)};
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

%% Keep going until we see a "->". FIXME, should be clever about "->"
%% appearing inside comments, atoms, or strings.
take_function_head(S) -> take_function_head(S, []).

take_function_head("->"++S, Acc) ->
    {ok, reverse(Acc), S};
take_function_head("%"++S0, Acc) ->
    {_, S1} = take_line(S0),
    take_function_head(S1, Acc);
take_function_head([H|T], Acc) ->
    take_function_head(T, [H|Acc]);
take_function_head([], _Acc) ->
    eos.

%% Returns: {Line, Rest}
take_line(S) -> take_line(S, []).

take_line([],      Acc) -> {reverse(Acc), []};
take_line([$\n|S], Acc) -> {reverse("\n"++Acc), S};
take_line([H|T],   Acc) -> take_line(T, [H|Acc]).

drop_line(S0) ->
    {_, S1} = take_line(S0),
    S1.

comments_to_doc([]) ->
    [];
    %% FIXME: we should really generate nothing here, since zillions
    %% of these "undocumented" strings will match e.g. an apropos
    %% search for "documented" :-) -luke
%    "(undocumented)\n";
comments_to_doc(Comments) ->
    flatten([Line || Line <- map(fun clean_comment/1, Comments),
		     not boring_comment(Line)]).

boring_comment("\n") -> true;
boring_comment(S) ->
    %% Matches "%% ----------" lines
    lists:all(fun(Ch) -> Ch == $- end, S -- "\n").

clean_comment("% "++S) -> S;
clean_comment("%"++S)  -> clean_comment(S);
clean_comment(S)       -> S.

indent(S, Pad) -> flatten([Pad | indent1(S, Pad)]).

indent1("\n"++S, Pad) -> ["\n", Pad, indent1(S, Pad)];
indent1([H|T], Pad)   -> [H|indent1(T, Pad)];
indent1([], _)        -> [].

