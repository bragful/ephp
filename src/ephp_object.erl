%% @doc
%% This module is intended to store the object collection based on an array.
%% The original PHP has an array to collect all of the instances from all of the
%% possible objects. When a new instance is generated, it's allocated in an
%% empty position of that array.
%%
%% The variables only referenciate to that object so, all of the variables
%% which contains objects are pointers indeed.
%% @end
-module(ephp_object).
-author('manuel@altenwald.com').
-compile([{no_auto_import, [get/1]}]).

-include("ephp.hrl").

-export([
    start_link/0,
    destroy/2,
    get/1,
    get/2,
    get_class_name/1,
    get_class_name/2,
    get_class/1,
    get_class/2,
    set_class/2,
    get_context/1,
    get_context/2,
    set/2,
    set/3,
    add/2,
    add_link/1,
    add_link/2,
    remove_all/2,
    remove/2,
    remove/3,
    clone/2,
    set_attr/3,
    set_bulk_attr/2
]).

set_attr(#obj_ref{pid = Objects, ref = ObjectId} = Object,
         #variable{name = AttrName} = Attr, Value) ->
    #ephp_object{context = Ctx, class = Class} = Obj = ephp_object:get(Object),
    ephp_context:set(Ctx, Attr, Value),
    NewClass = ephp_class:add_if_no_exists_attrib(Class, AttrName),
    ephp_object:set(Objects, ObjectId, Obj#ephp_object{class = NewClass}),
    ok.

set_bulk_attr(#obj_ref{pid = Objects, ref = ObjectId} = Object, Values) when is_list(Values) ->
    #ephp_object{context = Ctx, class = Class} = Obj = ephp_object:get(Object),
    Vars = [ AttrName || {#variable{name = AttrName}, _} <- Values ],
    NewClass = ephp_class:add_if_no_exists_attrib(Class, Vars),
    ephp_object:set(Objects, ObjectId, Obj#ephp_object{class = NewClass}),
    ephp_context:set_bulk(Ctx, Values),
    ok.

-spec start_link() -> {ok, ephp:objects_id()}.
%% @doc creates a new Objects storage.
start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, array:new()),
    {ok, Ref}.


-spec destroy(ephp:context_id(), ephp:objects_id()) -> ok.
%% @doc Destroy the Objects storage.
destroy(Context, Ref) ->
    remove_all(Context, Ref),
    erlang:erase(Ref),
    ok.


-spec get(ephp:objects_id(), object_id()) -> undefined | ephp_object().
%% @doc retrieves an object based on the Object ID.
get(Ref, ObjectId) ->
    Objects = erlang:get(Ref),
    array:get(ObjectId, Objects).


-spec get(obj_ref()) -> undefined | ephp_object().
%% @doc retrieves an object based on the Object ID.
get(undefined) ->
    undefined;
get(#obj_ref{pid = Objects, ref = ObjectId}) ->
    get(Objects, ObjectId).


-spec get_class_name(ephp:objects_id(), object_id()) -> class_name().
%% @doc retrieves the class name for a provided Object ID.
get_class_name(Objects, ObjectId) ->
    ObjectsData = erlang:get(Objects),
    ((array:get(ObjectId, ObjectsData))#ephp_object.class)#class.name.


-spec get_class_name(obj_ref()) -> class_name().
%% @equiv get_class_name/2
get_class_name(#obj_ref{pid = Objects, ref = ObjectId}) ->
    get_class_name(Objects, ObjectId).


-spec get_class(obj_ref()) -> class().
%% @doc retrieves the class record for a provided Object ID.
get_class(#obj_ref{pid = Objects, ref = ObjectId}) ->
    get_class(Objects, ObjectId).


-spec get_class(ephp:objects_id(), object_id()) -> class().
%% @doc retrieves the class record for a provided Object ID.
get_class(Objects, ObjectId) ->
    ObjectsData = erlang:get(Objects),
    (array:get(ObjectId, ObjectsData))#ephp_object.class.


-spec set_class(ephp:obj_ref(), class()) -> ok.
%% @doc set a class for an existent object.
set_class(#obj_ref{pid = Objects, ref = ObjectId}, Class) ->
    ObjectsData = erlang:get(Objects),
    Object = array:get(ObjectId, ObjectsData),
    NewObject = Object#ephp_object{class = Class},
    NewObjectsData = array:set(ObjectId, NewObject, ObjectsData),
    erlang:put(Objects, NewObjectsData),
    ok.


-spec get_context(ephp:objects_id(), object_id()) -> ephp:context_id().
%% @doc retrieves the object context for an provided Object ID.
get_context(Objects, ObjectId) ->
    ObjectsData = erlang:get(Objects),
    (array:get(ObjectId, ObjectsData))#ephp_object.context.


-spec get_context(obj_ref()) -> ephp:context_id().
%% @equiv get_context/2
get_context(#obj_ref{pid = Objects, ref = ObjectId}) ->
    get_context(Objects, ObjectId).


-spec set(obj_ref(), ephp_object()) -> ok.
%% @doc stores an object given the Object ID.
set(#obj_ref{pid = Objects, ref = ObjectId}, Object) ->
    set(Objects, ObjectId, Object).


-spec set(ephp:objects_id(), object_id(), ephp_object()) -> ok.
%% @doc stores an object given the Object ID.
set(Objects, ObjectId, Object) ->
    ObjectsData = erlang:get(Objects),
    NewObjects = array:set(ObjectId,
                           Object#ephp_object{id = ObjectId,
                                              objects = Objects}, ObjectsData),
    erlang:put(Objects, NewObjects),
    ok.


-spec add(ephp:objects_id(), ephp_object()) -> object_id().
%% @doc stores an object searching an empty slot for it and returning the
%%      Object ID where it was stored.
%% @end
add(Objects, Object) ->
    ObjectsData = erlang:get(Objects),
    ObjectId = search_empty(ObjectsData),
    set(Objects, ObjectId, Object),
    ObjectId.


-spec add_link(ephp:objects_id(), object_id()) -> ok.
%% @doc increases the number of links for an object.
add_link(Objects, ObjectId) ->
    #ephp_object{links = Links} = Object = get(Objects, ObjectId),
    set(Objects, ObjectId, Object#ephp_object{links = Links + 1}),
    ok.


-spec add_link(obj_ref()) -> ok.
%% @equiv add_link/2
add_link(#obj_ref{pid = Objects, ref = ObjectId}) ->
    add_link(Objects, ObjectId).


-spec remove_all(ephp:context_id(), ephp:objects_id()) -> ok.
%% @doc removes all of the objects stored using objects_id.
remove_all(Context, Objects) ->
    ObjectsData = erlang:get(Objects),
    array:foldl(fun(_, #ephp_object{id = ObjectId}, _) ->
                    remove_complete(Context, Objects, ObjectId);
                   (_, undefined, _) ->
                    ok
                end, undefined, ObjectsData),
    erlang:put(Objects, array:new()),
    ok.


-spec remove(ephp:context_id(), ephp:objects_id(), object_id()) -> ok.
%% @doc decreases the number of links for an object. If arrives to zero,
%%      the object is removed.
%% @end
remove(Context, Objects, ObjectId) ->
    case get(Objects, ObjectId) of
        undefined ->
            ok;
        #ephp_object{links = Links} = Object ->
            if
                Links =< 1 ->
                    remove_complete(Context, Objects, ObjectId);
                true ->
                    set(Objects, ObjectId, Object#ephp_object{links = Links - 1})
            end
    end,
    ok.


-spec remove(ephp:context_id(), obj_ref()) -> ok.
%% @equiv remove/3
remove(Context, #obj_ref{pid = Objects, ref = ObjectId}) ->
    remove(Context, Objects, ObjectId).


-spec remove_complete(ephp:context_id(), ephp:objects_id(), object_id()) -> ok.
%% @doc removes an object from the storage given the object or its ID.
remove_complete(Context, Objects, ObjectId) ->
    ObjRef = #obj_ref{pid = Objects, ref = ObjectId},
    #ephp_object{class = Class, context = Ctx} = get(ObjRef),
    Classes = ephp_context:get_classes(Context),
    case ephp_class:get_destructor(Classes, Class) of
        undefined ->
            ok;
        #class_method{name = Name, line = Line} ->
            Call = #call{name = Name, line = Line, type = object},
            ephp_context:call_method(Context, ObjRef, Call),
            ok
    end,
    Vars = ephp_context:get_vars(Ctx),
    ephp_vars:destroy(Context, Vars),
    NewObjects = array:set(ObjectId, undefined, erlang:get(Objects)),
    erlang:put(Objects, NewObjects),
    ok.


-spec clone(ephp:context_id(), obj_ref()) -> obj_ref().
%% @doc clones an object generating a new one and executing its `__clone'
%%      function by the way.
%% @end
clone(Context, #obj_ref{pid = Objects, ref = _ObjectId} = ObjRef) ->
    #ephp_object{class = Class, context = Ctx} = Object = get(ObjRef),
    ObjectsData = erlang:get(Objects),
    NewObjectId = search_empty(ObjectsData),
    {ok, ClonedContext} = ephp_context:clone(Ctx),
    NewObject = Object#ephp_object{context = ClonedContext,
                                   id = NewObjectId,
                                   links = 1},
    NewObjectsData = array:set(NewObjectId, NewObject, ObjectsData),
    erlang:put(Objects, NewObjectsData),
    NewObjRef = #obj_ref{pid = Objects, ref = NewObjectId},
    Classes = ephp_context:get_classes(Context),
    case ephp_class:get_clone(Classes, Class) of
        undefined ->
            ok;
        #class_method{name = Name, line = Line} ->
            Call = #call{name = Name, line = Line, type = object},
            ephp_context:call_method(Context, NewObjRef, Call),
            ok
    end,
    NewObjRef.


-spec search_empty(array:array()) -> object_id().
%% @doc search an empty slot and returns the Object ID.
search_empty(Objects) ->
    search_empty(1, Objects).

-spec search_empty(I :: pos_integer(), array:array()) -> object_id().
%% @doc search en empty slot and returns the Object ID. This function uses the
%%      first param as iterator to search the ID.
%% @end
search_empty(I, Objects) ->
    case array:get(I, Objects) of
        undefined -> I;
        _ -> search_empty(I+1, Objects)
    end.
