-module(ephp_object).
-author('manuel@altenwald.com').
-compile([warnings_as_errors, {no_auto_import, [get/1]}]).

%% @doc
%% This module is intended to store the object collection based on an array.
%% The original PHP has an array to collect all of the instances from all of the
%% possible objects. When a new instance is generated, it's allocated in an
%% empty position of that array.
%%
%% The variables only referenciate to that object so, all of the variables
%% which contains objects are pointers indeed.
%% @end

-include("ephp.hrl").

-export([
    start_link/0,
    destroy/2,
    get/1,
    get/2,
    get_class_name/1,
    get_class_name/2,
    get_context/1,
    get_context/2,
    set/3,
    add/2,
    add_link/1,
    add_link/2,
    remove_link/2,
    remove_link/3,
    remove_all/2,
    remove/2,
    remove/3
]).

-spec start_link() -> {ok, ephp:objects_id()}.
%% @doc creates a new Objects storage.
start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, array:new()),
    {ok, Ref}.


-spec destroy(context(), ephp:objects_id()) -> ok.
%% @private
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
get(#obj_ref{pid = Ref, ref = ObjectId}) ->
    get(Ref, ObjectId).


-spec get_class_name(ephp:objects_id(), object_id()) -> class_name().
%% @doc retrieves the class name for an provided Object ID.
get_class_name(Ref, ObjectId) ->
    Objects = erlang:get(Ref),
    ((array:get(ObjectId, Objects))#ephp_object.class)#class.name.


-spec get_class_name(obj_ref()) -> class_name().
%% @doc retrieves the class name for an provided Object ID.
get_class_name(#obj_ref{pid = Ref, ref = ObjectId}) ->
    get_class_name(Ref, ObjectId).


-spec get_context(obj_ref()) -> context().
%% @doc retrieves the object context for an provided Object ID.
get_context(#obj_ref{pid = Ref, ref = ObjectId}) ->
    get_context(Ref, ObjectId).


-spec get_context(ephp:objects_id(), object_id()) -> context().
%% @doc retrieves the object context for an provided Object ID.
get_context(Ref, ObjectId) ->
    Objects = erlang:get(Ref),
    (array:get(ObjectId, Objects))#ephp_object.context.


-spec set(ephp:objects_id(), object_id(), ephp_object()) -> ok.
%% @doc stores an object given the Object ID.
set(Ref, ObjectId, Object) ->
    Objects = erlang:get(Ref),
    NewObjects = array:set(ObjectId,
                           Object#ephp_object{id = ObjectId,
                                              objects = Ref}, Objects),
    erlang:put(Ref, NewObjects),
    ok.


-spec add(ephp:objects_id(), ephp_object()) -> object_id().
%% @doc stores an object searching an empty slot for it and returning the
%%      Object ID where it was stored.
%% @end
add(Ref, Object) ->
    Objects = erlang:get(Ref),
    ObjectId = search_empty(Objects),
    set(Ref, ObjectId, Object#ephp_object{id = ObjectId}),
    ObjectId.


-spec add_link(obj_ref()) -> ok.
%% @doc increases the number of links for an object.
add_link(#obj_ref{pid = Ref, ref = ObjectId}) ->
    add_link(Ref, ObjectId).


-spec add_link(ephp:objects_id(), object_id()) -> ok.
%% @doc increases the number of links for an object.
add_link(Ref, ObjectId) ->
    #ephp_object{links = Links} = Object = get(Ref, ObjectId),
    set(Ref, ObjectId, Object#ephp_object{links = Links + 1}),
    ok.


-spec remove_link(context(), obj_ref()) -> ok.
%% @doc decreases the number of links for an object. If arrives to zero,
%%      the object is removed.
%% @end
remove_link(Context, #obj_ref{pid = Ref, ref = ObjectId}) ->
    remove_link(Context, Ref, ObjectId).


-spec remove_all(context(), ephp:objects_id()) -> ok.
%% @doc removes all of the objects stored using objects_id.
remove_all(Context, Ref) ->
    Objects = erlang:get(Ref),
    array:foldl(fun(_, #ephp_object{id = ObjectId}, _) ->
                    remove(Context, Ref, ObjectId);
                   (_, undefined, _) ->
                    ok
                end, undefined, Objects),
    erlang:put(Ref, array:new()),
    ok.


-spec remove_link(context(), ephp:objects_id(), object_id()) -> ok.
%% @doc decreases the number of links for an object. If arrives to zero,
%%      the object is removed.
%% @end
remove_link(Context, Ref, ObjectId) ->
    case get(Ref, ObjectId) of
        undefined ->
            ok;
        #ephp_object{links = Links} = Object ->
            case Links of
                1 ->
                    remove(Context, Ref, ObjectId);
                _ ->
                    set(Ref, ObjectId, Object#ephp_object{links = Links - 1})
            end
    end,
    ok.


-spec remove(context(), obj_ref() | object_id()) -> ok.
%% @doc removes an object from the storage given the object or its ID.
remove(Context, #obj_ref{pid = Ref, ref = ObjectId}) ->
    remove(Context, Ref, ObjectId).


-spec remove(context(), ephp:objects_id(), ephp_object() | object_id()) -> ok.
%% @doc removes an object from the storage given the object or its ID.
remove(Context, Ref, #ephp_object{id = ObjectId}) ->
    remove(Context, Ref, ObjectId);
remove(Context, Ref, ObjectId) when is_integer(ObjectId) andalso ObjectId > 0 ->
    ObjRef = #obj_ref{pid = Ref, ref = ObjectId},
    #ephp_object{class = Class} = get(ObjRef),
    case ephp_class:get_destructor(Class) of
        undefined ->
            ok;
        #class_method{line = Line} ->
            Call = #call{name = <<"__destruct">>, line = Line},
            ephp_context:call_method(Context, ObjRef, Call),
            ok
    end,
    NewObjects = array:set(ObjectId, undefined, erlang:get(Ref)),
    erlang:put(Ref, NewObjects),
    ok.

-spec search_empty(ephp:objects_id()) -> object_id().
%% @doc search an empty slot and returns the Object ID.
search_empty(Objects) ->
    search_empty(1, Objects).

-spec search_empty(I :: pos_integer(), ephp:objects_id()) -> object_id().
%% @doc search en empty slot and returns the Object ID. This function uses the
%%      first param as iterator to search the ID.
%% @end
search_empty(I, Objects) ->
    case array:get(I, Objects) of
        undefined -> I;
        _ -> search_empty(I+1, Objects)
    end.
