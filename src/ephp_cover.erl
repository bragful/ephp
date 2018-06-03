%% @doc The cover module is in charge to store the information about the lines
%%      executed and the times. Finally it generates a XML file with that
%%      information in the coverage format. It uses the version 4.
%% @end
-module(ephp_cover).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-define(XML_HEAD, "<?xml version=\"1.0\"?>"
                  "<!--DOCTYPE coverage SYSTEM "
                  "\"http://cobertura.sourceforge.net/xml/coverage-04.dtd\"-->"
                  "<coverage timestamp=\"~b\" line-rate=\"~f\" "
                  "lines-covered=\"0\" lines-valid=\"0\" branch-rate=\"0.0\" "
                  "branches-covered=\"0\" branches-valid=\"0\" "
                  "complexity=\"0\" version=\"1.9.4.1\">"
                  "<sources>~s</sources>"
                  "<packages>~s</packages>"
                  "</coverage>").

-define(SOURCE, "<source>~s</source>").
-define(PACKAGE, "<package name=\"~s\" line-rate=\"~f\" branch-rate=\"0\" "
                 "complexity=\"0\"><classes>~s</classes></package>").
-define(CLASS, "<class name=\"~s\" filename=\"~s\" line-rate=\"~f\" "
               "branch-rate=\"0.0\" complexity=\"0\">"
                %% TODO methods should have some more information like...
                "<methods/>"
                "<lines>~s</lines>"
                "</class>").
-define(ENTRY, "<line number=\"~b\" hits=\"~b\"/>").

-export([
    start_link/0,
    init_file/3,
    store/3,
    store/4,
    dump/0,
    get_config/0
]).

-spec start_link() -> ok.
%% @doc ensure the cover information is in the process.
start_link() ->
    case erlang:get(cover) of
        undefined ->
            erlang:put(cover, []),
            ok;
        _ ->
            ok
    end.

-type is_cover() :: boolean().

-spec init_file(is_cover(),
                Filename :: binary(),
                Compiled :: [main_statement()]) -> ok.
%% @doc store the lines of the file inside of the cover information.
init_file(false, _Filename, _Compiled) ->
    ok;

init_file(true, Filename, Compiled) ->
    case erlang:get(cover) of
        undefined ->
            init_file0(Filename, Compiled, []);
        Files ->
            case lists:keyfind(Filename, 1, Files) of
                {Filename, _} ->
                    ok;
                false ->
                    init_file0(Filename, Compiled, Files)
            end
    end,
    ok.

-spec init_file0(Filename :: binary(),
                 Compiled :: [main_statement()],
                 Files :: [{File :: binary(), cover_dict()}]) -> ok.
%% @hidden
init_file0(Filename, Compiled, Files) ->
    Lines = process(Compiled, []),
    FileDict = lists:foldl(fun(N, D) ->
        orddict:store(N, 0, D)
    end, orddict:new(), Lines),
    erlang:put(cover, [{Filename, FileDict}|Files]),
    ok.


-type hits() :: non_neg_integer().
-type file_line() :: pos_integer().
-type cover_dict() :: [{file_line(), hits()}].

-spec process(Compiled :: [main_statement()],
              Lines :: cover_dict()) -> cover_dict().
%% @doc get the line information from commands and return the dictionary with
%%      lines.
%% @end
process([{{line,L},_}|Data], N) ->
    process(Data, [L|N]);

process([], N) ->
    lists:usort(N);

process([Data|Rest], N) when is_tuple(Data) ->
    NewN = process(erlang:tuple_to_list(Data), N),
    process(Rest, NewN);

process([Data|Rest], N) when is_list(Data) ->
    NewN = process(Data, N),
    process(Rest, NewN);

process([_|Data], N) ->
    process(Data, N).


-spec store(is_cover(),
            statement_type(),
            FileOrContext :: binary() | context(),
            line() | undefined) -> ok.
%% @doc store the information about the statement passed as a param. Calls to
%%      store/3 only if is_cover() (first param) is true.
%% @end
store(_Cover, _Type, _FileOrContext, undefined) ->
    ok;

store(true = _Cover, Type, FileOrContext, Line) ->
    store(Type, FileOrContext, Line);

store(false = _Cover, _Type, _FileOrContext, _Line) ->
    ok.


-type statement_type() :: print | eval | assign | if_block | switch |
                          switch_case | for | foreach | while |
                          {call, binary()} | pre_incr | post_incr |
                          pre_decr | post_decr | {op, Type :: atom()} |
                          class | function | global | return | int | float |
                          text | object | define | constant | switch_default.

-spec store(statement_type(),
            FileOrContext :: binary() | context(),
            line() | undefined) -> ok.
%% @doc store the information about the statement passed as a param.
store(Type, File, {_,L,C}) ->
    store(Type, File, {{line,L},{column,C}});

store(_Type, _File, undefined) ->
    ok;

store(_Type, File, {{line,Line},_}) when is_binary(File) ->
    Files = erlang:get(cover),
    NewFiles = orddict:update(File, fun(Dict) ->
        orddict:update_counter(Line, 1, Dict)
    end, [], Files),
    case ephp_config:get_bool(<<"cover.realtime">>, false) of
        true ->
            case whereis(ephp_config:get_atom(<<"cover.process">>)) of
                PID when is_pid(PID) -> PID ! {run, File, Line};
                _ -> io:format("=> ~s ~b~n", [File, Line])
            end;
        false ->
            ok
    end,
    erlang:put(cover, NewFiles),
    ok;

store(Type, Context, Line) when is_reference(Context) ->
    File = ephp_context:get_active_file(Context),
    store(Type, File, Line).


-spec dump() -> ok.
%% @doc dump the coverage information to the output file defined by
%%      cover.output or cobertura.xml by default.
%% @end
dump() ->
    Files = erlang:get(cover),
    Classes = dump(Files, []),
    Percentage = total_percentage(Files),
    Packages = io_lib:format(?PACKAGE, ["base", Percentage, Classes]),
    Sources = [ io_lib:format(?SOURCE, [dirname(File)]) || {File,_} <- Files ],
    Epoch = ephp_datetime:posix_time_ms(),
    XML = io_lib:format(?XML_HEAD, [Epoch, Percentage, Sources, Packages]),
    Output = case ephp_config:get(<<"cover.output">>) of
        undefined -> <<"cobertura.xml">>;
        Filename -> Filename
    end,
    file:write_file(Output, XML),
    ok.


-spec dump(Files :: [binary()], Output :: iolist()) -> iolist().
%% @hidden
dump([], Classes) ->
    lists:reverse(Classes);

dump([{File, Dict} | Cover], Classes) ->
    Percentage = percentage(Dict),
    Lines = lists:flatmap(fun({Line, Hits}) ->
        io_lib:format(?ENTRY, [Line, Hits])
    end, Dict),
    Class = io_lib:format(?CLASS, [name(File), basename(File), Percentage, Lines]),
    dump(Cover, [Class|Classes]).


-spec get_config() -> boolean().
%% @doc get information about whether cover is enabled.
get_config() ->
    case ephp_data:to_bool(ephp_config:get(<<"cover.enable">>)) of
        true -> true;
        _ -> false
    end.


-spec get_active_lines(cover_dict()) -> non_neg_integer().
%% @doc number of lines used in the running.
get_active_lines(Data) ->
    length(lists:filter(fun({_,0}) -> false; (_) -> true end, Data)).


-spec percentage(cover_dict()) -> float().
%% @doc percentage between 0 and 1 about the coverage.
percentage(Data) ->
    get_active_lines(Data) / length(Data).


-spec total_percentage(Files :: [{File :: binary(), cover_dict()}]) -> float().
%% @doc percentage total for all of the files between 0 and 1 about the
%%      coverage.
%% @end
total_percentage(Files) ->
    {A, T} = lists:foldl(fun({_, Dict}, {A,T}) ->
        {A + get_active_lines(Dict), T + length(Dict)}
    end, {0, 0}, Files),
    A / T.


-spec dirname(Filename :: binary()) -> binary().
%% @hidden
dirname(Filename) ->
    filename:dirname(Filename).


-spec basename(Filename :: binary()) -> binary().
%% @hidden
basename(Filename) ->
    filename:basename(Filename).


-spec name(Filename :: binary()) -> binary().
%% @hidden
name(Filename) ->
    filename:rootname(filename:basename(Filename)).
