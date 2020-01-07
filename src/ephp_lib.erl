%% @doc 
%% 
-module(ephp_lib).
-author('manuel@altenwald.com').

-include("ephp.hrl").

% init_conf
-type php_config_results() :: [{config_section(), config_param()}].
-type config_section() :: binary().
-type config_param() :: mixed().

% init_const
-type php_const_results() :: [{const_name(), number() | binary()}].
-type const_name() :: binary().

% init_func
-type php_function_results() :: [php_function_defs()].

-type php_function_defs() :: php_function() |
                             {php_function(), php_function_opts()}.

-type php_function() :: atom().
-type php_function_opts() :: [php_function_opt()].
-type php_function_opt() :: pack_args |
                            {namespace, namespace()} |
                            {alias, php_function_alias()} |
                            php_function_args().

-type php_function_alias() :: binary().

-type php_function_args() :: {args, validation_args()}.

-type default_value() :: mixed().
-type error_return_value() :: mixed().
-type min_args() :: non_neg_integer().
-type max_args() :: non_neg_integer().
-type type_arg() :: mixed |
                    string |
                    integer |
                    array |
                    object |
                    resource |
                    raw |
                    type_ref.
-type validation_arg() ::
    {type_arg(), default_value()} |
    type_arg().
-type validation_args() ::
    {min_args(), max_args(), error_return_value(), [validation_arg()]} |
    [validation_arg()] |
    undefined |
    no_resolve.

-export_type([
    php_function/0,
    php_function_results/0,
    php_function_opts/0,
    php_function_opt/0,

    php_const_results/0,

    config_section/0,
    config_param/0,
    php_config_results/0,
    validation_args/0
]).

-callback init_func() -> php_function_results().

-callback init_config() -> php_config_results().

-callback init_const() -> php_const_results().

-callback handle_error(ephp_error:error_type(), ephp_error:error_level(),
                       Args::term()) -> string() | ignore.

-callback get_classes() -> [class()].

-optional_callbacks([handle_error/3, get_classes/0]).

-export([
    register/2
]).

-spec register(context(), module()) -> ok.
%% @doc register a module.
%% @see ephp_func
register(Ctx, Module) ->
    ephp_config:module_init(Module),
    maybe_register_errors(Ctx, Module),
    maybe_register_classes(Ctx, Module),
    register_functions(Ctx, Module),
    ok.

maybe_register_errors(Ctx, Module) ->
    case erlang:function_exported(Module, handle_error, 3) of
        true -> ephp_error:add_message_handler(Ctx, Module);
        false -> ok
    end.

maybe_register_classes(Ctx, Module) ->
    case erlang:function_exported(Module, get_classes, 0) of
        true ->
            ClassRef = ephp_context:get_classes(Ctx),
            Classes = Module:get_classes(),
            ephp_class:register_classes(ClassRef, Ctx, Classes);
        false ->
            ok
    end.

register_functions(Ctx, Module) ->
    lists:foreach(fun(Func) ->
        register_func(Ctx, Module, Func)
    end, Module:init_func()).

register_func(Ctx, Module, {Func, Opts}) ->
    PackArgs = proplists:get_value(pack_args, Opts, false),
    NameBin = atom_to_binary(Func, utf8),
    Name = proplists:get_value(alias, Opts, NameBin),
    Args = proplists:get_value(args, Opts),
    NS = proplists:get_value(namespace, Opts, []),
    ephp_context:register_func(Ctx, NS, Name, Module, Func, PackArgs, Args);
register_func(Ctx, Module, Func) ->
    Name = atom_to_binary(Func, utf8),
    ephp_context:register_func(Ctx, [], Name, Module, Func, false, undefined).
