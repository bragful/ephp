-module(ephp_lib_class).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    handle_error/3,
    get_class/3,
    class_alias/4
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {get_class, [{args, {1, 1, false, [object]}}]},
    class_alias
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

-spec handle_error(ephp_error:error_type(), ephp_error:error_level(),
                   Args::term()) -> string() | ignore.

handle_error(enoclassscope, _Level, {<<"self">>}) ->
    "Cannot access self:: when no class scope is active";

handle_error(eundefclass, _Level, {Class}) ->
    io_lib:format("Class '~s' not found", [ephp_data:to_bin(Class)]);

handle_error(eprivateaccess, _Level, {Class, Element, Access}) ->
    io_lib:format(
        "Cannot access ~s property ~s::$~s",
        [Access, Class, Element]);

handle_error(ecallprivate, _Level, {Class, Element, Access}) ->
    io_lib:format("Call to ~s method ~s::~s()", [Access, Class, Element]);

handle_error(eredefinedclass, _Level, {ClassName}) ->
    io_lib:format("Cannot redeclare class ~s", [ClassName]);

handle_error(eassignthis, _Level, {}) ->
    "Cannot re-assign $this";

handle_error(enostatic, _Level, {Class, Method}) ->
    io_lib:format("Non-static method ~s::~s() should not be called statically",
                  [Class, Method]);

handle_error(enoclone, _Level, {}) ->
    "__clone method called on non-object";

handle_error(enoconst, _Level, {ConstName}) ->
    io_lib:format("Undefined class constant '~s'", [ConstName]);

handle_error(enointerface, _Level, {InterfaceName}) ->
    io_lib:format("Interface '~s' not found", [InterfaceName]);

handle_error(enomethods, _Level, {ClassName, Methods, Params}) ->
    io_lib:format("Class ~s contains ~b abstract methods and must therefore "
                  "be declared abstract or implement the remaining methods "
                  "(~s)", [ClassName, Params, iolist_to_binary(Methods)]);

handle_error(ecannotimpl, _Level, {ClassName, NoInterfaceName}) ->
    io_lib:format("~s cannot implement ~s - it is not an interface",
                  [ClassName, NoInterfaceName]);

handle_error(eimpl, _Level, {PrevClass, Method, Class, Args}) ->
    io_lib:format("Declaration of ~s::~s() must be compatible with ~s::~s(~s)",
                  [PrevClass, Method, Class, Method, Args]);

handle_error(_Type, _Level, _Args) ->
    ignore.

-spec get_class(context(), line(), Class :: var_value()) -> any().

get_class(_Context, _Line, {_, #obj_ref{pid = Objects, ref = ObjectId}}) ->
    ephp_object:get_class_name(Objects, ObjectId).

-spec class_alias(context(), line(),
                  ClassName :: var_value(),
                  ClassAlias :: var_value()) -> boolean().

class_alias(Context, Line, {_,Name}, {_,Alias}) ->
    case ephp_context:set_class_alias(Context, Name, Alias) of
        ok ->
            true;
        {error, enoexist} ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefclass, Line, File, ?E_WARNING, {Name}}),
            false;
        {error, eredefined} ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eredefinedclass, Line, File, ?E_WARNING, {Alias}}),
            false
    end.
