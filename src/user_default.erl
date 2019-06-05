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
-define(BUILD_LIB, "_build/**/lib").

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
    case catch
        [begin
             case catch find_source(Mod) of
                 undefined ->
                     throw({error, {no_mod, Mod}});
                 {ok, SourceList} ->
                     ?PRINT("~ncompiling: ~s", [SourceList]),
                     [compile(Mod, Source) || Source <- SourceList]
             end
         end || Mod <- Mods]
    of
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

find_source(Mod) ->
    FileName = atom_to_list(Mod) ++ ".erl",
    case filelib:wildcard(filename:join([?BUILD_LIB, "**", FileName])) of
        [] -> undefined;
        SourceList -> {ok, SourceList}
    end.

compile(Mod, Source) ->
    SrcDir = filename:dirname(Source),
    BaseDir = filename:dirname(SrcDir),
    EbinDir = filename:join([BaseDir, "ebin"]),
    Opts = [{outdir, EbinDir} | ?COMPILE_OPTS],
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
