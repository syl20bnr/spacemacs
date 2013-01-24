%% -*- erlang-indent-level: 2 -*-
%%% Created :  6 Mar 2008 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('otp_doc').
-author('Mats Cronqvist').

-define(is_str(S),(S==[] orelse is_integer(hd(S)))).
%% --------------------------------------------------------------------------
%% gen_server boilerplate
-behaviour(gen_server).
-export([handle_call/3, handle_cast/2, handle_info/2, 
         init/1, terminate/2, code_change/3]).
%% --------------------------------------------------------------------------
% API - these run in the shell
-export([start/0,start/1,stop/0]).
-export([distel/4,modules/1,functions/2,arguments/2]).
-export([firefox/1,firefox/2,firefox/3]).
-export([sig/1,sig/2,sig/3]).
-export([funcsf/3]).

stop() ->
  case whereis(?MODULE) of
    undefined -> ok;
    _ -> gen_server:call(?MODULE,stop)
  end.

start() -> start([]).
start(Props) -> assert(Props).

distel(link,M,F,A) when ?is_str(M),?is_str(F),is_integer(A) -> 
  get(link,M,F,A);
distel(sig,M,F,A) when ?is_str(M),?is_str(F),is_integer(A) ->
  case get(sig,M,F,A) of
    [] -> [];
    no_html -> no_html;
    Sigs -> {sig, str_join(Sigs,"\n")}
  end;
distel(A,B,C,D) ->
  erlang:display({A,B,C,D}),
  [].

sig(M) -> sig(M,'').
sig(M,F) -> sig(M,F,-1).
sig(M,F,A) when is_atom(M), is_atom(F), is_integer(A) -> get(sig,M,F,A).

firefox(M) -> firefox(M,'').
firefox(M,F) -> firefox(M,F,-1).
firefox(M,F,A) when is_atom(M), is_atom(F), is_integer(A) -> 
  case get(link,M,F,A) of
    no_html -> no_doc_installed;
    Link -> ffx(Link)
  end.
  
ffx({link,Link}) -> os:cmd("firefox "++Link),Link;
ffx({mfas,MFAs}) -> MFAs;
ffx([]) -> no_doc.

modules(Prefix) -> completions(mods,Prefix,"").
functions(Mod,Prefix) -> completions(funcs,Mod,Prefix).
arguments(_Mod,_Fun) -> {ok,""}.

completions(What,M,F) ->
  case get(What,M,F,"") of
    no_html -> {error,no_html};
    [] -> {error,not_found};
    Ans -> {ok,Ans}
  end.

get(W,M,F,A) when is_atom(W) ->
  assert([]),
  gen_server:call(?MODULE,{W,to_list(M),to_list(F),to_list(A)}).

assert(Props) ->
  case whereis(?MODULE) of
    undefined -> gen_server:start({local,?MODULE},?MODULE,Props,[]);
    Pid -> {ok,Pid}
  end.

%% --------------------------------------------------------------------------
%% implementation - runs in the server
-record(state,{root_dir,prot=file,delim=delim()}).

%% gen_server callbacks
init(Props) -> 
  Dir =  proplists:get_value(root_dir, Props, code:root_dir()),
  Prot = proplists:get_value(prot, Props, file),
  ets:new(?MODULE,[named_table,ordered_set]),
  try html_index(Prot,Dir),
      {ok,#state{root_dir=Dir,prot=Prot}}
  catch _:_ -> 
      {ok,no_html}
  end.

terminate(_,_) -> ok.
code_change(_,State,_) -> {ok,State}.
handle_cast(_In,State) -> {noreply,State}.
handle_info(_In,State) -> {noreply,State}.

handle_call(stop,_From,State) -> 
  {stop,normal,ok,State};
handle_call(_In,_From,no_html) ->
  {reply,no_html,no_html};
handle_call({sig,M,F,A},_From,State) -> 
  {reply,handle_sig(M,F,A,State),State};
handle_call({mods,M,_,_},_From,State) -> 
  {reply,handle_mods(M,State),State};
handle_call({funcs,M,F,_},_From,State) -> 
  {reply,handle_funcs(M,F,State),State};
handle_call({link,M,F,A},_From,State) -> 
  {reply,handle_link(M,F,A,State),State}.

%% --------------------------------------------------------------------------
handle_sig(Mo,Fu,Aa,_State) ->
  try [get_sig(M,F,A) || {M,F,A} <- matching_mfas(Mo, Fu, Aa)]
  catch _:_ -> []
  end.

handle_mods(Prefix,_State) ->
  try all_prefix_keys({file,Prefix})
  catch _:_ -> []
  end.

handle_funcs(Mod,Prefix,_State) ->
  try [F || {_,F} <- all_fs(Mod,Prefix)]
  catch _:_ -> []
  end.

handle_link(Mo,Fu,Aa,State) ->
  try MFAs = matching_mfas(Mo, Fu, Aa),
      until([fun()->link_with_anchor(MFAs,State) end,
	     fun()->exact_match(Mo,Fu,MFAs,State) end,
	     fun()->link_without_anchor(MFAs,State) end,
	     fun()->mfa_multi(MFAs,State) end])
  catch _:_ -> []
  end.

until([F|Fs]) ->
  try F()
  catch _:_ -> until(Fs)
  end.

link_with_anchor(MFAs,State) -> 
  [_] = lists:usort([{M,F}||{M,F,_A}<-MFAs]),
  {A,{M,F}} = lists:min([{A,{M,F}}||{M,F,A}<-MFAs]),
  {link,io_str("~w://~s#~s~s~s",
	 [State#state.prot,e_get({file,M}),linkmf(M,F),State#state.delim,A])}.

linkmf("erlang",F) -> "erlang:"++F;
linkmf(_,F) -> F.

exact_match(M,F,MFAs,State) -> 
  A = lists:min([A||{Mo,Fu,A}<-MFAs,M==Mo,F==Fu]),
  {link,io_str("~w://~s#~s~s~s",
	 [State#state.prot,e_get({file,M}),linkmf(M,F),State#state.delim,A])}.

link_without_anchor(MFAs,State) -> 
  [M] = lists:usort([M || {M,_F,_A} <- MFAs]),
  {link,io_str("~w://~s",[State#state.prot, e_get({file,M})])}.

mfa_multi(MFAs,_State) ->
  {mfas,str_join([io_str("~s:~s/~s",[M,F,A])||{M,F,A}<-MFAs],", ")}.

get_sig(M,F,A) ->
  Sig = e_get({{sig,M,F},A}),
  io_str("~s:~s",[M,Sig]).

matching_mfas(Mo, Fu, Aa) ->
  MFs = lists:append([all_fs(M,Fu) || M <- matching_ms(Mo)]),
  lists:append([[{M,F,A} || A <- which_a(M,F,Aa)] || {M,F} <- MFs]).

matching_ms(M) ->
  Ms = all_prefix_keys({file,M}),
  case lists:member(M,Ms) of 
    true -> [M];
    false-> Ms
  end.

all_fs(Mo,Fu) ->
  maybe_cache(Mo),
  [{Mo,F} || F <- all_prefix_keys({{as,Mo},Fu})].

all_prefix_keys({Tag,X}) ->
  case ets:member(?MODULE,{Tag,X}) of
    true -> [X|all_prefix_keys({Tag,X},ets:next(?MODULE,{Tag,X}))];
    false-> all_prefix_keys({Tag,X},ets:next(?MODULE,{Tag,X}))
  end.

all_prefix_keys({Tag,X0},{Tag,X}) ->
  case lists:prefix(X0,X) of
    true -> [X|all_prefix_keys({Tag,X0},ets:next(?MODULE,{Tag,X}))];
    false-> []
  end;
all_prefix_keys(_,_) ->
  [].

which_a(M,F,"-1") -> 
  e_get({{as,M},F});
which_a(M,F,A) ->
  case lists:member(A,e_get({{as,M},F})) of
    true -> [A];
    false-> []
  end.

%% --------------------------------------------------------------------------
%% read the index file
%% store name of html file in {Mod,file}
html_index(file,Dir) ->
  FN = filename:join([Dir,"doc","man_index.html"]),
  fold_file(curry(fun lines/3,Dir),[],FN).

lines(Line,_,Dir) ->
  case string:tokens(Line, "<> \"") of
    ["TD", "A", "HREF=", "../"++Href, M|_] -> 
      case filename:basename(Href,".html") of
	"index" -> ok;
	M -> e_set({file,M}, filename:join([Dir,Href]))
      end;
    _ -> ok
  end.

%% --------------------------------------------------------------------------
%% read a module's html file
%% store the function names in {fs,Mod}, the arities in {{as,M},F} and the 
%% function signature in {{sig,M,F},A}

maybe_cache(M) ->
  try e_get({fs,M}) 
  catch 
    no_data -> cache_funcs(M)
  end.

cache_funcs(M) ->
  e_set({fs,M},[]),
  fold_file(curry(fun funcsf/3,M), [], e_get({file,M})).

funcsf(Line,A,M) ->
  case trim_P(string:tokens(A++Line,"<>\"")) of
    ["a name=",FA,"/a","span class=","bold_code",Sig,"/span"|_] ->  % R14
      a_line(M,fa(FA),Sig),[];			% R12-
    ["a name=",FA,"span class=","bold_code",Sig,"/span","/a"|_] ->
      a_line(M,fa(FA),Sig),[];			% R12-
    ["A NAME=",FA,"STRONG","CODE",Sig,"/CODE","/STRONG","/A"|_] ->
      a_line(M,fa(FA),Sig),[];			% -R11
    ["A NAME=",_,"STRONG","CODE"|_] ->
      A++Line;					% -R11, broken lines
    _ -> 
      case A of
	[] -> [];
	_ -> A++Line
      end
  end.

a_line(_,["Module:"++_,_],_) -> ok;  %ignore the gen_server/gen_fsm callbacks
a_line("erlang",["erlang:"++F,A],"erlang:"++Sig) -> a_line("erlang",[F,A],Sig);
a_line(M,[F,A],Sig) ->	    %io:fwrite("- ~p~n",[{M,F,A}]).
  try e_bag({fs,M},F),
      e_bag({{as,M},F}, A),
      e_set({{sig,M,F},A}, dehtml(Sig))
  catch _:_ -> ok
  end.

fa(FA) -> string:tokens(FA,delim()).

trim_P([_,"P"|L]) -> L;
trim_P([_,"p"|L]) -> L;
trim_P(["P"|L])   -> L;
trim_P(["p"|L])   -> L;
trim_P(L)         -> L.

delim() ->
  try erlang:system_info(otp_release), "-"
  catch _:_ -> "/"
  end.

%% --------------------------------------------------------------------------
%% ets-based dictionary
e_set(Key,Val) -> ets:insert(?MODULE,{Key,Val}).

e_get(Key) ->
  case ets:lookup(?MODULE,Key) of
    [{Key,Val}] -> Val;
    [] -> throw(no_data);
    X -> exit({bad_e_get,{table,?MODULE},{key,Key},{value,X}})
  end.

e_bag(Key,Val) ->
  try e_get(Key) of
    L when is_list(L) -> e_set(Key,[Val|L]);
    X                 -> exit({bad_e_bag,{table,?MODULE},{key,Key},{value,X}})
  catch
    no_data -> e_set(Key,[Val])
  end.

%% --------------------------------------------------------------------------
%% the missing fold_file/3 function
fold_file(Fun,Acc0,File) ->
  {ok, FD} = file:open(File, [read]),
  Acc = fold_file_lines(FD,Fun,Acc0),
  file:close(FD),
  Acc.

fold_file_lines(FD,Fun,Acc) ->
  case io:get_line(FD, "") of
    eof -> Acc;
    Line -> fold_file_lines(FD,Fun,Fun(trim_nl(Line),Acc))
  end.

trim_nl(Str) -> lists:reverse(tl(lists:reverse(Str))).
  
%% --------------------------------------------------------------------------
%% Schönfinkelisation
curry(F,Arg) ->
  case erlang:fun_info(F,arity) of
    %% {_,1} -> fun() -> F(Arg) end;
    %% {_,2} -> fun(A) -> F(A,Arg) end;
    {_,3} -> fun(A,B) -> F(A,B,Arg) end
    %% {_,4} -> fun(A,B,C) -> F(A,B,C,Arg) end
  end.

%% --------------------------------------------------------------------------
io_str(F,A) -> lists:flatten(io_lib:format(F,A)).

%% dehtmlize right angle
dehtml("&#62;"++Str) -> ">"++dehtml(Str);
dehtml("&gt;"++Str)  -> ">"++dehtml(Str);
dehtml([H|Str])      -> [H|dehtml(Str)];
dehtml("")           -> "".

str_join([], _Sep) -> "";
str_join([Pref|Toks], Sep) ->
  lists:foldl(fun(Tok,O) -> O++Sep++Tok end, Pref, Toks).

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(I) when is_integer(I)-> integer_to_list(I);
to_list(S) when ?is_str(S) -> S.
