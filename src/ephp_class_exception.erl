-module(ephp_class_exception).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    get_class/0,
    get_trace/1,
    get_message/1,
    exception_construct/4,
    exception_get_message/3,
    exception_get_code/3,
    exception_get_file/3,
    exception_get_line/3,
    exception_get_trace/3,
    exception_get_previous/3,
    exception_get_trace_as_string/3,
    exception_to_string/3
]).

-import(ephp_class, [class_attr/2, class_attr/3]).

get_class() ->
    #class{
        name = <<"Exception">>,
        constants = dict:new(),
        attrs = [
            class_attr(<<"message">>, protected, <<"Unknown exception">>),
            class_attr(<<"string">>, private, <<>>),
            class_attr(<<"code">>, protected, 0),
            class_attr(<<"file">>, protected),
            class_attr(<<"line">>, protected),
            class_attr(<<"trace">>, private, ephp_array:new()),
            class_attr(<<"previous">>, private)
        ],
        methods = [
            #class_method{
                name = <<"__construct">>,
                code_type = builtin,
                args = [
                    #variable{name = <<"message">>, default_value = <<>>},
                    #variable{name = <<"code">>, default_value = 0},
                    #variable{name = <<"previous">>,
                              data_type = <<"Exception">>}
                ],
                builtin = {?MODULE, exception_construct},
                pack_args = true
            },
            #class_method{
                name = <<"__clone">>,
                code_type = builtin,
                args = [],
                access = private,
                final = true
            },
            #class_method{
                name = <<"getMessage">>,
                code_type = builtin,
                args = [],
                builtin = {?MODULE, exception_get_message},
                final = true
            },
            #class_method{
                name = <<"getCode">>,
                code_type = builtin,
                args = [],
                builtin = {?MODULE, exception_get_code},
                final = true
            },
            #class_method{
                name = <<"getFile">>,
                code_type = builtin,
                args = [],
                builtin = {?MODULE, exception_get_file},
                final = true
            },
            #class_method{
                name = <<"getLine">>,
                code_type = builtin,
                args = [],
                builtin = {?MODULE, exception_get_line},
                final = true
            },
            #class_method{
                name = <<"getTrace">>,
                code_type = builtin,
                args = [],
                builtin = {?MODULE, exception_get_trace},
                final = true
            },
            #class_method{
                name = <<"getPrevious">>,
                code_type = builtin,
                args = [],
                builtin = {?MODULE, exception_get_previous},
                final = true
            },
            #class_method{
                name = <<"getTraceAsString">>,
                code_type = builtin,
                args = [],
                builtin = {?MODULE, exception_get_trace_as_string},
                final = true
            },
            #class_method{
                name = <<"__toString">>,
                code_type = builtin,
                args = [],
                builtin = {?MODULE, exception_to_string}
            }
        ]
    }.

get_trace(#reg_instance{context = ClassCtx}) ->
    ephp_context:get(ClassCtx, #variable{name = <<"trace">>}).

get_message(#reg_instance{context = ClassCtx}) ->
    ephp_context:get(ClassCtx, #variable{name = <<"message">>}).

exception_construct(Ctx, #reg_instance{context = ClassCtx}, {{line,Line},_},
                    [{_, Message}, {_, Code}, {_, Previous}]) ->
    File = ephp_context:get_active_file(Ctx),
    Traces = ephp_stack:get_array(Ctx),
    ephp_context:set(ClassCtx, #variable{name = <<"message">>}, Message),
    ephp_context:set(ClassCtx, #variable{name = <<"code">>}, Code),
    ephp_context:set(ClassCtx, #variable{name = <<"previous">>}, Previous),
    ephp_context:set(ClassCtx, #variable{name = <<"file">>}, File),
    ephp_context:set(ClassCtx, #variable{name = <<"line">>}, Line),
    ephp_context:set(ClassCtx, #variable{name = <<"trace">>}, Traces),
    Message.

exception_get_message(_Ctx, #reg_instance{context = ClassCtx}, _Line) ->
    ephp_context:get(ClassCtx, #variable{name = <<"message">>}).

exception_get_code(_Ctx, #reg_instance{context = ClassCtx}, _Line) ->
    ephp_context:get(ClassCtx, #variable{name = <<"code">>}).

exception_get_file(_Ctx, #reg_instance{context = ClassCtx}, _Line) ->
    ephp_context:get(ClassCtx, #variable{name = <<"file">>}).

exception_get_line(_Ctx, #reg_instance{context = ClassCtx}, _Line) ->
    ephp_context:get(ClassCtx, #variable{name = <<"line">>}).


exception_get_trace(_Ctx, #reg_instance{context = ClassCtx}, _Line) ->
    ephp_context:get(ClassCtx, #variable{name = <<"trace">>}).


exception_get_previous(_Ctx, #reg_instance{context = ClassCtx}, _Line) ->
    ephp_context:get(ClassCtx, #variable{name = <<"previous">>}).


exception_get_trace_as_string(_Ctx, #reg_instance{context = _ClassCtx}, _L) ->
    %% TODO
    <<>>.

exception_to_string(_Ctx, #reg_instance{context = ClassCtx}, _Line) ->
    ephp_context:get(ClassCtx, #variable{name = <<"file">>}).
