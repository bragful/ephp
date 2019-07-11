-module(ephp_class_closure).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    get_classes/0,

    closure_construct/4,
    closure_invoke/4
]).

-import(ephp_class, [class_attr/2, class_attr/3]).

-spec get_classes() -> [class()].

get_classes() -> [
    #class{
        name = <<"Closure">>,
        attrs = [],
        methods = [
            #class_method{
                name = <<"__construct">>,
                code_type = builtin,
                args = [],
                builtin = {?MODULE, closure_construct},
                pack_args = true
            },
            %% TODO: bindTo(object $newThis [, mixed $newscope = 'static']) : Closure
            %% TODO: call(object $newThis [, mixed $newscope = 'static']) : mixed
            %% TODO: static bind(Closure $closure, object $newThis [, mixed $newscope = 'static']) : Closure
            %% TODO: static fromCallable(callable $callable) : Closure
            #class_method{
                name = <<"__invoke">>,
                code_type = builtin,
                args = [],
                builtin = {?MODULE, closure_invoke},
                pack_args = true,
                validation_args = no_resolve
            }
        ]
    }
].

closure_construct(_Ctx, _ObjRef, _Line, []) ->
    ok.

closure_invoke(Ctx, ObjRef, Line, Params) ->
    ObjCtx = ephp_object:get_context(ObjRef),
    AnonFunc = ephp_context:get_meta(ObjCtx, invoke),
    OriginParams = [ Name || {Name, _} <- Params ],
    Call = #call{name = AnonFunc, args = OriginParams, line = Line},
    ephp_context:solve(Ctx, Call).
