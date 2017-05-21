-module(ephp_cover).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-define(XML_HEAD, "<?xml version=\"1.0\"?>"
                  "<!--DOCTYPE coverage SYSTEM "
                  "\"http://cobertura.sourceforge.net/xml/coverage-03.dtd\"-->"
                  "<coverage timestamp=\"~b\" line-rate=\"0.0\" "
                  "lines-covered=\"0\" lines-valid=\"0\" branch-rate=\"0.0\""
                  "branches-covered=\"0\" branches-valid=\"0\" "
                  "complexity=\"0\" version=\"1.9.4.1\">"
                  "<sources>~s</sources>"
                  "<packages>~s</packages>"
                  "</coverage>").

-define(SOURCE, "<source>~s</source>").
-define(PACKAGE, "<package name=\"~s\" line-rate=\"~f\" branch-rate=\"0\""
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

start_link() ->
    case erlang:get(cover) of
        undefined ->
            erlang:put(cover, []),
            ok;
        _ ->
            ok
    end.

init_file(false, _Filename, _Compiled) ->
    ok;
init_file(true, Filename, Compiled) ->
    Lines = process(Compiled, []),
    File = lists:foldl(fun(N, D) ->
        orddict:store(N, 0, D)
    end, orddict:new(), Lines),
    Files = erlang:get(cover),
    erlang:put(cover, [{Filename, File}|Files]),
    ok.

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

store(_Cover, _Type, _FileOrContext, undefined) ->
    ok;
store(true = _Cover, Type, FileOrContext, Line) ->
    store(Type, FileOrContext, Line);
store(false = _Cover, _Type, _FileOrContext, _Line) ->
    ok.

store(Type, File, {_,L,C}) ->
    store(Type, File, {{line,L},{column,C}});
store(_Type, _File, undefined) ->
    ok;
store(_Type, File, {{line,Line},_}) when is_binary(File) ->
    Files = erlang:get(cover),
    NewFiles = orddict:update(File, fun(Dict) ->
        orddict:update_counter(Line, 1, Dict)
    end, Files),
    erlang:put(cover, NewFiles),
    ok;
store(Type, Context, Line) when is_reference(Context) ->
    File = ephp_context:get_active_file(Context),
    store(Type, File, Line).

dump() ->
    Files = erlang:get(cover),
    Classes = dump(Files, []),
    Percentage = total_percentage(Files),
    Packages = io_lib:format(?PACKAGE, ["base", Percentage, Classes]),
    Sources = [ io_lib:format(?SOURCE, [File]) || {File,_} <- Files ],
    XML = io:format(?XML_HEAD, [epoch(), Sources, Packages]),
    %% TODO: dump in the file indicated in the configuration
    io:format("~s~n", [XML]),
    ok.

dump([], Classes) ->
    lists:reverse(Classes);
dump([{File, Dict} | Cover], Classes) ->
    Percentage = percentage(Dict),
    Lines = lists:flatmap(fun({Line, Hits}) ->
        io_lib:format(?ENTRY, [Line, Hits])
    end, Dict),
    Class = io_lib:format(?CLASS, [name(File), File, Percentage, Lines]),
    dump(Cover, [Class|Classes]).

get_config() ->
    case ephp_data:to_bool(ephp_config:get(<<"cover">>)) of
        true -> true;
        _ -> false
    end.

get_active_lines(Data) ->
    length(lists:filter(fun({_,0}) -> false; (_) -> true end, Data)).

percentage(Data) ->
    get_active_lines(Data) / length(Data).

total_percentage(Files) ->
    {A, T} = lists:foldl(fun({_, Dict}, {A,T}) ->
        {A + get_active_lines(Dict), T + length(Dict)}
    end, {0, 0}, Files),
    A / T.

name(Filename) ->
    filename:rootname(filename:basename(Filename)).

epoch() ->
    {A,B,_} = os:timestamp(),
    A * 1000000 + B.
