-module(ephp_object).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

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
    destroy/1,
    get/2,
    get_class_name/2,
    set/3,
    add/2,
    remove/2
]).

-type object_id() :: pos_integer().

-spec start_link() -> {ok, ephp:objects_id()}.
%% @doc creates a new Objects storage.
start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, array:new()),
    {ok, Ref}.


-spec destroy(ephp:objects_id()) -> ok.
%% @private
%% @doc Destroy the Objects storage.
destroy(Objects) ->
    % TODO: maybe call to the destruct
    erlang:erase(Objects),
    ok.


-spec get(ephp:objects_id(), object_id()) -> undefined | ephp_object().
%% @doc retrieves an object based on the Object ID.
get(Ref, ObjectId) ->
    Objects = erlang:get(Ref),
    array:get(ObjectId, Objects).


-spec get_class_name(ephp:objects_id(), object_id()) -> class_name().
%% @doc retrieves the class name for an provided Object ID.
get_class_name(Ref, ObjectId) ->
    Objects = erlang:get(Ref),
    ((array:get(ObjectId, Objects))#ephp_object.class)#class.name.


-spec set(ephp:objects_id(), object_id(), ephp_object()) -> ok.
%% @doc stores an object given the Object ID.
set(Ref, ObjectId, Object) ->
    Objects = erlang:get(Ref),
    NewObjects = array:set(ObjectId, Object#ephp_object{id = ObjectId}, Objects),
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


-spec remove(ephp:objects_id(), ephp_object() | object_id()) -> ok.
%% @doc removes an object from the storage given the object or its ID.
remove(Ref, #ephp_object{id = ObjectId}) ->
    remove(Ref, ObjectId);
remove(Ref, ObjectId) when is_integer(ObjectId) andalso ObjectId > 0 ->
    Objects = erlang:get(Ref),
    NewObjects = array:set(ObjectId, undefined, Objects),
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
