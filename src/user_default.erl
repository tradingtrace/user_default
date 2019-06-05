%%%-------------------------------------------------------------------
%%% @author TradingTrace
%%% @copyright (C) 2019
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2019 19:55
%%%-------------------------------------------------------------------
-module(user_default).
-author("tradingtrace@outlook.com").

%% API
-export([ q/0
        , uc/1
        , ul/1
        , ucl/1
        , load/1
        ]).

-define(PRINT(Format),
    io:format(Format++"~n", [])).
-define(PRINT(Format, Args),
    io:format(Format++"~n", Args)).

-define(COMPILE_OPTS, [debug_info]).
-define(REBAR_BUILD, "_build").
-define(REBAR_CHECKOUTS, "_checkouts").

%%====================================================================
%% API functions
%%====================================================================
q() ->
    ?PRINT("WARNING!!! You should never use this command...~n\"^G q\" to quit this shell.").

%% Compile one or more modules
uc(Mod) when is_atom(Mod) ->
    uc([Mod]);
uc(Mods) ->
    compile_and_load(Mods, false).

%% Load one or more modules
ul(Mod) when is_atom(Mod) ->
    ul([Mod]);
ul(Mods) ->
    Nodes = [node()|nodes()],
    ?PRINT("~nupdate module: ~w~nupdate nodes: ~w", [Mods, Nodes]),
    [rpc:call(Node, ?MODULE, load, [Mods]) || Node <- Nodes].

%% Compile and reload one or more modules
ucl(Mod) when is_atom(Mod) ->
    ucl([Mod]);
ucl(Mods) ->
    compile_and_load(Mods, true).

%%====================================================================
%% Internal functions
%%====================================================================
compile_and_load(Mods, IsUpdate) ->
    case catch [compile(Mod, find_source(Mod)) || Mod <- Mods] of
        {error, _} = Errors ->
            Errors;
        Result ->
            case IsUpdate of
                true ->
                    ul(Mods),
                    ?PRINT("~n====== compile and update complete! ======",[]);
                false ->
                    ?PRINT("~n====== compile complete! ======",[])
            end,
            Result
    end.

load([]) -> ok;
load([FileName | T]) ->
    case c:l(FileName) of
        {module, _} ->
            load(T);
        {error, Error} ->
            {Error, FileName}
    end;
load(FileName) when is_atom(FileName) ->
    load([FileName]).

compile(Mod, Source) ->
    Opts = [{outdir, find_outdir(Mod)} | ?COMPILE_OPTS],
    case compile:file(Source, Opts) of
        {ok, _Data} ->
            ?PRINT("compile success: ~p!", [_Data]),
            ok;
        {ok, _, _Warnings} ->
            ?PRINT("compile success!", []),
            ?PRINT("warnings:~n~p", [_Warnings]),
            ok;
        error ->
            throw({error, {compile_failed, Mod}});
        {error, Errors, Warnings} ->
            throw({error, {compile_failed, Mod, {error, Errors}, {warnings, Warnings}}})
    end.

find_source(Mod) ->
    CompileInfo = Mod:module_info(compile),
    case proplists:get_value(source, CompileInfo) of
        undefined ->
            throw({error, {compile_failed, source_not_found, Mod}});
        Source -> Source
    end.

find_outdir(Mod) ->
    case code:is_loaded(Mod) of
        {file, "" ++ BeamPath} ->
            filename:dirname(BeamPath);
        false ->
            throw({error, {compile_failed, not_loaded, Mod}});
        {file, Atom} ->
            throw({error, {compile_failed, Atom, Mod}})
    end.