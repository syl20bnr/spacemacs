%
%%% distel_ie - an interactive erlang shell
%%%
%%% Some of the code has shamelessly been stolen from Luke Gorrie 
%%% [luke@bluetail.com] - ripped from its elegance and replaced by bugs. 
%%% It just goes to show that you can't trust anyone these days. And
%%% as if that wasn't enough, I'll even blame Luke: "He _made_ me do it!"
%%%
%%% So, without any remorse, I hereby declare this code to be:
%%%
%%% copyright (c) 2002 david wallin [david.wallin@ul.ie].
%%%
%%% (it's probably going to be released onto an unexpecting public under
%%%  some sort of BSD license).

-module(distel_ie).

-export([
	 evaluate/2,

	 test1/0,
	 test2/0,
	 test3/0,
	 test4/0,
	 test5/0,

	 ensure_registered/0,

	 start/0,
	 start/1,
	 init/1,
	 loop/1
	]).

-define(FMT(X), list_to_binary(lists:flatten(io_lib:format("~p", [X])))).


%%
%% ensure_registered/0
ensure_registered() ->
    case whereis(distel_ie) of
 	undefined -> start() ;
 	Pid  ->
	    group_leader(group_leader(), Pid),
	    ok
    end.

%%
%% start/0

start() ->
    start([]).


%%
%% start/1

start(Options) ->
    spawn(?MODULE, init, [Options]).


%%
%% init/1

init(_Options) ->
    register(distel_ie, self()),
    Defs = ets:new(definitions, [set]),
    Line = 12,
    Bindings = [],
    State = {Defs, Line, Bindings},
    loop(State).


%%
%% loop/1

loop({Defs, Line, Bindings}) ->
    receive 
	{evaluate, Emacs, String} -> 
	    case catch evaluate(String, {Defs, Line, Bindings}) of
		{'EXIT', Rsn} ->
		    Emacs ! {ok, list_to_binary(
				   io_lib:format("EXIT: ~p", [Rsn]))},
		    ?MODULE:loop({Defs, Line, Bindings});
		{Result, {NL, NB}} ->
		    Emacs ! Result,
		    ?MODULE:loop({Defs, NL, NB})
	    end;
	forget_bindings ->
	    put(distel_ie_bindings, []),
	    ?MODULE:loop({Defs, Line, []}) ;
	Unknown ->
	    io:format("distel_ie: unknown message recvd '~p'\n", [Unknown]),
	    ?MODULE:loop({Defs, Line, Bindings})

    end.


%%
%% evaluate/2

evaluate(String, {Defs, Line, Bindings}) ->
    case parse_expr(String) of
	%% ok, so it is an expression :
	{ok, Parse} ->
	    RemoteParse = add_remote_call_info(Parse, Defs),
            case catch erl_eval:exprs(RemoteParse, Bindings) of
                {value, V, NewBinds} ->
		    {{ok, ?FMT(V)}, {Line, NewBinds}};
                Error ->
		    {?FMT(Error), {Line, Bindings}}
	    end;
	%% try and treat it as a form / definition instead :
	Other ->
	    case parse_form(String) of
		{ok, Parse} -> 
		    {ok, Name, Arity} = get_function_name(Parse),
		    ets:insert(Defs, {{Name,Arity}, Parse}),
		    FunTrees = lists:flatten(
				 lists:reverse(ets:match(Defs,{'_', '$1'}))),
		    %% Line isn't really used yet
		    NewLine = Line,
		    compile_load(FunTrees),
		    Def = list_to_binary(atom_to_list(Name) ++ "/" ++ 
					 integer_to_list(Arity)),
		    {{ok, Def}, {NewLine, Bindings}} ;
		Error ->
		    {{error, ?FMT({Error, Other})}, {Line, Bindings}}
	    end
    end.

%%
%% parse_expr/1

parse_expr(String) ->
    case erl_scan:string(String) of
	{ok, Tokens, _} ->
	    catch erl_parse:parse_exprs(Tokens) ;
	{error, {_Line, erl_parse, Rsn}} ->
	    {error, lists:flatten(Rsn)};
	{error, Error, _} ->
	    {error, Error}
    end.

%%
%% parse_form/1

parse_form(String) ->
    case erl_scan:string(String) of
	{ok, Tokens, _} ->
	    catch erl_parse:parse_form(Tokens) ;
	{error, {_Line, erl_parse, Rsn}} ->
	    {error, lists:flatten(Rsn)};
	{error, Error, _} ->
	    {error, Error}
    end.


%%
%% defun/1

defun(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    {ok, Parse} = erl_parse:parse_form(Tokens),
    compile_load([Parse]).


%%
%% compile_load/1

compile_load(Parse) ->

    Header = [{attribute,9,module,distel_ie_internal},
	      {attribute,11,compile,export_all},
	      {attribute,12,export,[]}],

    EOF = [{eof,20}],

    SyntaxTree = Header ++ Parse ++ EOF,

    {ok, Mod, Binary} = compile:forms(SyntaxTree),
    File = "heltigenomfelfelsomfansomentyskindianenbefjadradgerman",
    code:load_binary(Mod, File, Binary).


%%
%% add_remote_call_info/2
%%
%% TODO: this is gonna need more work, e.g. it needs to recurse into 
%% lists (cons) and tuples ... +more

add_remote_call_info([], _Defs) -> [] ;
add_remote_call_info({var, L, Var}, _Defs) ->
    {var, L, Var} ;
add_remote_call_info({atom, L, Atom}, _Defs) ->
    {atom, L, Atom} ;
add_remote_call_info({integer, L, Value}, _Defs) ->
    {integer, L, Value} ;
add_remote_call_info({string, L, String}, _Defs) ->
    {string, L, String} ;

add_remote_call_info([{lc, L, Body, Gen}|Rs], Defs) ->
    Bd = add_remote_call_info(Body, Defs),
    Gn = add_remote_call_info(Gen, Defs),
    [{lc, L, Bd, Gn}|add_remote_call_info(Rs,Defs)];
add_remote_call_info([{generate, L, Var, Gen}|Gs], Defs) ->
    Gn = add_remote_call_info(Gen, Defs),
    [{generate, L, Var, Gn}|add_remote_call_info(Gs, Defs)];
add_remote_call_info({call, L, {atom, L2, Name}, Body}, Defs) ->
    hd(add_remote_call_info([{call, L, {atom, L2, Name}, Body}], Defs));

add_remote_call_info([{call, L, {atom, L2, Name}, Body} | Rs], Defs) ->
    B = add_remote_call_info(Body, Defs),
    IsBuiltin = erlang:is_builtin(erlang, Name, length(B)),
    Call = case IsBuiltin of 
	       true ->
		   {call, L, {atom, L2, Name}, B} ;
	       false ->
		   Arity = length(Body),
		   case find_module(Name, Arity) of 
		       {ok, Mod} ->
	 		   {call, L, {remote, L2, 
				      {atom, L2, Mod}, 
				      {atom, L2, Name}}, B} ;
		       {error, _} ->
			   {call, L, {atom, L2, Name}, B}
		   end
	   end,
    [Call | add_remote_call_info(Rs, Defs)] ;

add_remote_call_info([{tuple, L, Values} | Rs], Defs) ->
    F = fun(X) -> add_remote_call_info(X, Defs) end,
    [{tuple, L, lists:map(F, Values)} | add_remote_call_info(Rs, Defs)] ;


add_remote_call_info([{Type, L, Hdr, Body} | Rs], Defs) when is_list(Body) ->
    B = add_remote_call_info(Body, Defs),
    [{Type, L, Hdr, B} | add_remote_call_info(Rs, Defs)] ;

add_remote_call_info([{Type, L, Hd, Tl} | Rs], Defs) ->
    [{Type, L, Hd, Tl} | add_remote_call_info(Rs, Defs)] ;

add_remote_call_info([R | Rs], Defs) ->
    [add_remote_call_info(R, Defs) | add_remote_call_info(Rs, Defs) ];

add_remote_call_info(X, _Defs) ->
    X.


%%
%% find_module/2

find_module(Function, Arity) ->
    Mods = [distel_ie_internal, distel_ie, c],
    F = fun(M) -> not is_exported(Function, Arity, M) end,
    case lists:dropwhile(F, Mods) of
	[] ->
	    search_modules(Function, Arity, code:all_loaded()) ;
	[M | _] ->
	    {ok, M}
    end.


%%
%% is_exported/3

is_exported(Function, Arity, Module) ->
    case code:is_loaded(Module) of
	false ->
	    false;
	_ ->
	    Info = Module:module_info(),
	    {value, {exports, Exports}} = lists:keysearch(exports, 1, Info),
	    lists:member({Function, Arity}, Exports)
    end.


%%
%% search_modules/3

search_modules(_Function, _Arity, []) ->
    {error, not_found};
search_modules(Function, Arity, [{M, _} | Ms]) ->
    case is_exported(Function, Arity, M) of
	true ->
	    {ok, M} ;
	false ->
	    search_modules(Function, Arity, Ms)
    end.
    

%%
%% get_function_name/1

get_function_name({function, _, Name, Arity, _}) -> 
    {ok, Name, Arity} ;

get_function_name(Unknown) ->
    {error, Unknown}.


%%% ------------------------------------------------------------------- [tests]
    
%%
%% test1/0

test1() ->
    Defun = "sista(W) -> lists:last(W).",
    defun(Defun).

%%
%% test2/0

test2() ->
    
    Prefix = [
	      {attribute,9,module,compile_and_load_me},
	      {attribute,11,compile,export_all},
	      {attribute,12,export,[]}
	     ],
    
    Postfix = [{eof,20}],
    
    String = "sista([]) -> [] ;\n\nsista(W) -> lists:last(W).\n",
%    String = "sista([], _) -> [] ;\n\nsista(W, _) -> lists:last(W).\n",
    
    {ok, Tokens, _} = erl_scan:string(String, 23),
    {ok, Tree} = erl_parse:parse_form(Tokens),
    
    io:format("tree : '~p'\n", [Tree]),
    
    SyntaxTree = Prefix ++ [Tree] ++ Postfix,
    
    io:format("syntaxtree : '~p'\n", [SyntaxTree]),
    
    {ok, Mod, Binary} = compile:forms(SyntaxTree),
    code:load_binary(Mod, "spam", Binary),
    compile_and_load_me:sista([1,2,galapremiere]).


%%
%% test3/0

test3() ->

    Sista = "sista(W) -> lists:last(W).\n",
    defun(Sista),
    distel_ie_internal:sista([1,2,galapremiere]),
    NySista = "en_till_sista(W) -> lists:last(W).\n",
    defun(NySista),
    distel_ie_internal:en_till_sista([1,2,onestepbeyond]).


%%
%% test4/0

test4() ->

    Defs = ets:new(definitions, [set]),

    Define = "nisse([]) -> [] ;\n\nnisse(W) -> lists:last(W).",
    Expr = "nisse([1,2,3]).",
%    Expr = "distel_ie_internal:nisse([1,2,3]).",
%    Expr = "lists:last([1,2,3]).",
    
    D = evaluate(Define, {Defs, 10, []}),
    io:format("nisse defined: '~p'\n", [D]),
    io:format("is_loaded: '~p'\n", [code:is_loaded(distel_ie_internal)]),
    evaluate(Expr, {Defs, 10, []}).


%%
%% test5/0

test5() ->

    Defs = ets:new(definitions, [set]),
    
    Define = "nisse([]) -> [] ;\n\nnisse(W) -> lists:last(W).",
    Expr = "nisse([1,2,3]).",

    evaluate(Define, {Defs, 10, []}),
%    ets:insert(Defs, {nisse, nil}),

    {ok, Tokens, _} = erl_scan:string(Expr, 23),
    {ok, Tree} = erl_parse:parse_exprs(Tokens),

    io:format("original tree : '~p'\n", [Tree]),
    
    RemoteTree = add_remote_call_info(Tree, Defs),
    io:format("remote tree : '~p'\n", [RemoteTree]).

