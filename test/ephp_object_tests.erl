-module(ephp_object_tests).

-author('manuel@altenwald.com').

-compile([warnings_as_errors]).

-include_lib("eunit/include/eunit.hrl").

-include("ephp.hrl").

set_get_and_remove_test() ->
    {ok, Ctx} = ephp_context:start_link(),
    Objects = ephp_context:get_objects(Ctx),
    Id = 1,
    Class = #class{},
    Class2 = #class{name = <<"x">>},
    {ok, ObjCtx} = ephp_context:start_link(),
    Object =
        #ephp_object{id = Id,
                     objects = Objects,
                     class = Class,
                     context = ObjCtx},
    ObjRef = #obj_ref{pid = Objects, ref = Id},
    ?assertEqual(Id, ephp_object:add(Objects, #ephp_object{class = Class, context = ObjCtx})),
    ?assertEqual(Object, ephp_object:get(ObjRef)),
    ?assertEqual(ok, ephp_object:add_link(ObjRef)),
    ?assertEqual(ok, ephp_object:remove(Ctx, Objects, Id)),
    ?assertEqual(Object, ephp_object:get(ObjRef)),
    ?assertEqual(ok, ephp_object:remove(Ctx, Objects, Id)),
    ?assertEqual(undefined, ephp_object:get(ObjRef)),
    ?assertEqual(ok, ephp_object:set(Objects, 1, Object#ephp_object{class = Class2})),
    ?assertEqual(Object#ephp_object{class = Class2}, ephp_object:get(ObjRef)),
    ?assertEqual(ok, ephp_object:destroy(Ctx, Objects)),
    ?assertException(error, badarg, ephp_object:get(ObjRef)),
    ok.
