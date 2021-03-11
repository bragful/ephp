-module(ephp_lib_class).

-author('manuel@altenwald.com').

-behaviour(ephp_lib).

-export([init_func/0, init_config/0, init_const/0, handle_error/3, get_class/3,
         class_alias/4, class_exists/4, interface_exists/4]).

-include("ephp.hrl").

-spec init_func() -> ephp_lib:php_function_results().
init_func() ->
    [{get_class, [{args, {1, 1, false, [object]}}]},
     class_alias,
     {class_exists, [{args, {1, 2, false, [string, {boolean, true}]}}]},
     {interface_exists, [{args, {1, 2, false, [string, {boolean, true}]}}]}].

-spec init_config() -> ephp_lib:php_config_results().
init_config() ->
    [].

-spec init_const() -> ephp_lib:php_const_results().
init_const() ->
    [].

-spec handle_error(ephp_error:error_type(), ephp_error:error_level(), Args :: term()) ->
                      string() | ignore.
handle_error(enoclassscope, _Level, {<<"self">>}) ->
    "Cannot access self:: when no class scope is active";
handle_error(efinalclass, _Level, {FName, Name}) ->
    io_lib:format("Class ~s may not inherit from final class (~s)", [Name, FName]);
handle_error(eincompatctx, _Level, {Class, Method}) ->
    io_lib:format("Non-static method ~s::~s() cannot be called "
                  "statically, assuming $this from incompatible context",
                  [Class, Method]);
handle_error(eundefclass, _Level, {NS, ClassName}) when is_binary(ClassName) ->
    io_lib:format("Class '~s' not found", [ephp_ns:to_bin(NS, ClassName)]);
handle_error(eundefclass, _Level, {ClassName}) when is_binary(ClassName) ->
    io_lib:format("Class '~s' not found", [ClassName]);
handle_error(eundefmethod, _Level, {Class, MethodName}) ->
    io_lib:format("Call to undefined method ~s::~s()", [Class, MethodName]);
handle_error(eundefattr, _Level, {{private, Attr, _NS, _Class}, ClassName}) ->
    io_lib:format("Undefined property: ~s::$~s", [ClassName, Attr]);
handle_error(eundefattr, _Level, {Attr, ClassName}) when is_binary(Attr) ->
    io_lib:format("Undefined property: ~s::$~s", [ClassName, Attr]);
handle_error(eindirectmod, _Level, {Attr, ClassName}) when is_binary(Attr) ->
    io_lib:format("Indirect modification of overloaded property ~s::$~s "
                  "has no effect",
                  [ClassName, Attr]);
handle_error(eaccesslevel, _Level, {Class, Name, Access, OtherClass}) ->
    io_lib:format("Access level to ~s::$~s must be ~s (as in class ~s)",
                  [Class, Name, Access, OtherClass]);
handle_error(eprivateaccess, _Level, {Class, Element, Access}) ->
    io_lib:format("Cannot access ~s property ~s::$~s", [Access, Class, Element]);
handle_error(ecallprivate, _Level, {Class, Element, Access}) ->
    io_lib:format("Call to ~s method ~s::~s()", [Access, Class, Element]);
handle_error(ecallprivate, _Level, {<<>>, Class, Element, Access}) ->
    io_lib:format("Call to ~s method ~s::~s() from invalid context",
                  [Access, Class, Element]);
handle_error(ecallprivate, _Level, {CClass, Class, Element, Access}) ->
    io_lib:format("Call to ~s method ~s::~s() from context '~s'",
                  [Access, Class, Element, CClass]);
handle_error(eredefinedclass, _Level, {ClassName}) ->
    io_lib:format("Cannot redeclare class ~s", [ClassName]);
handle_error(eassignthis, _Level, {}) ->
    "Cannot re-assign $this";
handle_error(enostatic, _Level, {Class, Method}) ->
    io_lib:format("Non-static method ~s::~s() should not be called statically",
                  [Class, Method]);
handle_error(efinalmethod, _Level, {Class, Method}) ->
    io_lib:format("Cannot override final method ~s::~s()", [Class, Method]);
handle_error(enoclone, _Level, {}) ->
    "__clone method called on non-object";
handle_error(enoconst, _Level, {_NS, ConstName}) ->
    io_lib:format("Undefined class constant '~s'", [ConstName]);
handle_error(enointerface, _Level, {InterfaceName}) ->
    io_lib:format("Interface '~s' not found", [InterfaceName]);
handle_error(enomethods, _Level, {ClassName, Methods, 1}) ->
    io_lib:format("Class ~s contains 1 abstract method and must therefore "
                  "be declared abstract or implement the remaining methods "
                  "(~s)",
                  [ClassName, iolist_to_binary(Methods)]);
handle_error(enomethods, _Level, {ClassName, Methods, Params}) ->
    io_lib:format("Class ~s contains ~b abstract methods and must therefore "
                  "be declared abstract or implement the remaining methods "
                  "(~s)",
                  [ClassName, Params, iolist_to_binary(Methods)]);
handle_error(ecannotimpl, _Level, {ClassName, NoInterfaceName}) ->
    io_lib:format("~s cannot implement ~s - it is not an interface",
                  [ClassName, NoInterfaceName]);
handle_error(ecannotextends, _Level, {ClassName, Interface}) ->
    io_lib:format("Class ~s cannot extend from interface ~s", [ClassName, Interface]);
handle_error(eimpl, _Level, {PrevClass, Method, Class, Args}) ->
    io_lib:format("Declaration of ~s::~s() must be compatible with ~s::~s(~s)",
                  [PrevClass, Method, Class, Method, Args]);
handle_error(edupinterface, _Level, {Class, Interface}) ->
    io_lib:format("Class ~s cannot implement previously implemented interface ~s",
                  [Class, Interface]);
handle_error(enoobjthis, _Level, {}) ->
    "Using $this when not in object context";
handle_error(eobj4empty, _Level, undefined) ->
    "Creating default object from empty value";
handle_error(enamespace, _Level, undefined) ->
    "Namespace declaration statement has to be the very first statement or after any declare call in the script";
handle_error(enamespaceblock, _Level, undefined) ->
    "No code may exist outside of namespace {}";
handle_error(enamespacemix, _Level, undefined) ->
    "Cannot mix bracketed namespace declarations with unbracketed namespace declarations";
handle_error(_Type, _Level, _Args) ->
    ignore.

-spec get_class(ephp:context_id(), line(), Class :: var_value()) -> any().
get_class(_Context, _Line, {_, #obj_ref{pid = Objects, ref = ObjectId}}) ->
    ephp_object:get_class_name(Objects, ObjectId).

-spec class_alias(ephp:context_id(),
                  line(),
                  ClassName :: var_value(),
                  ClassAlias :: var_value()) ->
                     boolean().
class_alias(Context, Line, {_, Name}, {_, Alias}) ->
    {ClassNS, ClassName} =
        if is_binary(Name) ->
               ephp_ns:parse(Name);
           true ->
               {[], ephp_data:to_bin(Name)}
        end,
    {AliasNS, AliasName} =
        if is_binary(Alias) ->
               ephp_ns:parse(Alias);
           true ->
               {[], ephp_data:to_bin(Alias)}
        end,
    case ephp_context:set_class_alias(Context, ClassNS, ClassName, AliasNS, AliasName) of
        ok ->
            true;
        {error, enoexist} ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                                    {error,
                                     eundefclass,
                                     Line,
                                     File,
                                     ?E_WARNING,
                                     {ClassNS, ClassName}}),
            false;
        {error, eredefined} ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                                    {error, eredefinedclass, Line, File, ?E_WARNING, {Alias}}),
            false
    end.

-spec class_exists(ephp:context_id(),
                   line(),
                   Class :: var_value(),
                   AutoLoad :: var_value()) ->
                      boolean().
class_exists(Context, _Line, {_, Class}, {_, AutoLoad}) ->
    {ClassNS, ClassName} = ephp_ns:parse(Class),
    case ephp_class:get(Context, ClassNS, ClassName, AutoLoad) of
        {ok, #class{type = Type}} ->
            Type =/= interface;
        {error, enoexist} ->
            false
    end.

-spec interface_exists(ephp:context_id(),
                       line(),
                       Class :: var_value(),
                       AutoLoad :: var_value()) ->
                          boolean().
interface_exists(Context, _Line, {_, Class}, {_, AutoLoad}) ->
    {ClassNS, ClassName} = ephp_ns:parse(Class),
    case ephp_class:get(Context, ClassNS, ClassName, AutoLoad) of
        {ok, #class{type = interface}} ->
            true;
        _ ->
            false
    end.
