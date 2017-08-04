-module(ephp_interface).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    destroy/1,

    get/2,
    get_consts/1,
    get_consts/2,

    check_methods/2,
    register_interface/3
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, dict:new()),
    ok = register_interfaces(Ref),
    {ok, Ref}.

destroy(Interfaces) ->
    erlang:erase(Interfaces),
    ok.

get(Ref, InterfaceName) when is_reference(Ref) ->
    Interfaces = erlang:get(Ref),
    get(Interfaces, InterfaceName);

get(Interfaces, InterfaceName) ->
    case dict:find(InterfaceName, Interfaces) of
        {ok, Interface} ->
            {ok, Interface};
        error ->
            {error, enoexist}
    end.

get_consts(#interface{constants = Constants}) ->
    Constants.

get_consts(Ref, InterfaceName) ->
    {ok, Interface} = get(Ref, InterfaceName),
    Interface#interface.constants.

check_methods(Interface, ClassMethods) ->
    case check_methods(Interface, ClassMethods, []) of
        [] -> ok;
        Missing -> {missing, lists:reverse(Missing)}
    end.

check_methods(#interface{methods = []}, _ClassMethods, Error) ->
    Error;
check_methods(#interface{methods = [Method|Methods]} = I, ClassMethods, Error) ->
    Res = lists:any(fun(#class_method{name = Name, args = Args}) ->
        (Name =:= Method#class_method.name) and
        (length(Args) =:= length(Method#class_method.args))
    end, ClassMethods),
    Rest = I#interface{methods = Methods},
    NewError = case Res of
        true -> Error;
        false -> [{I#interface.name, Method}|Error]
    end,
    check_methods(Rest, ClassMethods, NewError).

register_interface(Ref, File,
                   #interface{name = Name} = PHPInterface) ->
    Interfaces = erlang:get(Ref),
    ActivePHPInterface = PHPInterface#interface{
        file = File
    },
    erlang:put(Ref, dict:store(Name, ActivePHPInterface, Interfaces)),
    ok.

%% ------------------------------------------------------------------
%% Private functions
%% ------------------------------------------------------------------

register_interfaces(Ref) ->
    Interfaces = [
    ],
    InterfacesDict = lists:foldl(fun(#interface{name = Name} = Interface, Dict) ->
        dict:store(Name, Interface, Dict)
    end, erlang:get(Ref), Interfaces),
    erlang:put(Ref, InterfacesDict),
    ok.

