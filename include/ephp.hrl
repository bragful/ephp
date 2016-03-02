
-ifdef(FAST_DICT_MODE).
-define(DICT, dict).
-define(IS_DICT(D), (element(1, D) =:= dict)).
-else.
-define(DICT, orddict).
%% FIXME: do a better way to do this:
-define(IS_DICT(D), (is_list(D))).
-endif.

-ifndef(OLD_DICT_TYPE).
-define(DICT_TYPE, (dict:dict())).
-else.
-define(DICT_TYPE, (dict())).
-endif.

-define(PHP_INI_FILE, <<"php.ini">>).
-define(PHP_VERSION, <<"5.2.0">>).

-define(PATH_SEP, <<":">>).

-define(FUNC_ANON_NAME, <<"{closure}">>).

% built-in modules
-define(MODULES, [
    ephp_lib_date,
    ephp_lib_vars,
    ephp_lib_misc,
    ephp_lib_ob,
    ephp_lib_control,
    ephp_lib_array,
    ephp_lib_string,
    ephp_lib_file,
    ephp_lib_func,
    ephp_lib_info,
    ephp_lib_class
]).

% built-in consts
-define(CONST_MODULES, [
    ephp_error
]).

-define(E_ERROR, 1).
-define(E_WARNING, 2).
-define(E_PARSE, 4).
-define(E_NOTICE, 8).
-define(E_CORE_ERROR, 16).
-define(E_CORE_WARNING, 32).
-define(E_COMPILE_ERROR, 64).
-define(E_COMPILE_WARNING, 128).
-define(E_USER_ERROR, 256).
-define(E_USER_WARNING, 512).
-define(E_USER_NOTICE, 1024).
-define(E_STRICT, 2048).
-define(E_RECOVERABLE_ERROR, 4096).
-define(E_DEPRECATED, 8192).
-define(E_USER_DEPRECATED, 16384).
-define(E_ALL, 32767).

-type error_level() :: pos_integer().

-type date() :: {Year :: integer(), Month :: integer(), Day :: integer()}.

-type file_name() :: binary().

-type mixed() ::
    integer() | float() | binary() | boolean() | ?DICT_TYPE | null.

-type var_value() :: {variable(), mixed()}.

-type context() :: reference().

-type statement() :: tuple() | atom().
-type statements() :: [statement()].

-type expression() :: operation().

-type reason() :: atom() | string().

-type line() :: {{line, non_neg_integer()}, {column, non_neg_integer()}}.

% main statements

-record(eval, {
    statements :: statements(),
    line :: line()
}).

-record(print, {
    expression :: expression(),
    line :: line()
}).

-record(print_text, {
    text :: binary(),
    line :: line()
}).

-type main_statement() :: #eval{} | #print{} | #print_text{}.

% blocks

-record(if_block, {
    conditions :: conditions(),
    true_block :: statements(),
    false_block :: statements(),
    line :: line()
}).

-record(for, {
    init :: expression(),
    conditions :: conditions(),
    update :: expression(),
    loop_block :: statements(),
    line :: line()
}).

-record(while, {
    type :: (pre | post),
    conditions :: conditions(),
    loop_block :: statements(),
    line :: line()
}).

-record(foreach, {
    kiter :: variable(),
    iter :: variable(),
    elements :: variable(),
    loop_block :: statements(),
    line :: line()
}).

-record(switch_case, {
    label :: default | mixed(),
    code_block :: statements(),
    line :: line()
}).

-type switch_case() :: #switch_case{}.
-type switch_cases() :: [switch_case()].

-record(switch, {
    condition :: condition(),
    cases :: switch_cases(),
    line :: line()
}).

-type if_block() :: #if_block{}.

% data types and operations

-type ternary() :: if_block().

-record(operation, {
    type :: binary() | atom(),
    expression_left :: variable(),
    expression_right :: expression(),
    line :: line()
}).

-type operation_not() :: {operation_not, condition()}.

-type condition() :: expression() | operation().
-type conditions() :: [condition()].

-type operation() :: #operation{}.

-type arith_mono() :: pre_incr() | pre_decr() | post_incr() | post_decr().

-type array_index() :: arith_mono() | ternary() | binary() | operation().

-type post_decr() :: {post_decr, variable(), line()}.
-type pre_decr() :: {pre_decr, variable(), line()}.
-type post_incr() :: {post_incr, variable(), line()}.
-type pre_incr() :: {pre_incr, variable(), line()}.

-type return() :: {return, expression(), line()}.

-record(int, {
    int :: integer(),
    line :: line()
}).

-record(float, {
    float :: float(),
    line :: line()
}).

-record(text, {
    text :: binary(),
    line :: line()
}).

-record(text_to_process, {
    text :: [expression() | variable() | binary()],
    line :: line()
}).

-type constant_types() :: normal | class | define.

-record(constant, {
    name :: binary(),
    type = normal :: constant_types(),
    value :: expression(),
    class :: class_name() | undefined,
    line :: line()
}).

-type constant() :: #constant{}.

-type variable_types() :: normal | object | class.

-record(variable, {
    type = normal :: variable_types(),
    class :: class_name() | undefined,
    name :: binary(),
    idx = [] :: [array_index() | {object, binary()} | {class, binary()}],
    default_value = null :: mixed(),
    line :: line()
}).

-type variable() :: #variable{}.

-record(array_element, {
    idx = auto :: auto | expression(),
    element :: expression(),
    line :: line()
}).

-type array_element() :: #array_element{}.

-record(array, {
    elements = [] :: [array_element()],
    line :: line()
}).

-type php_array() :: #array{}.

% statements

-record(assign, {
    variable :: variable(),
    expression :: expression(),
    line :: line()
}).

-type call_types() :: normal | class | object.
-type class_name() :: binary().

-record(call, {
    type = normal :: call_types(),
    class :: undefined | class_name(),
    name :: binary(),
    args = [] :: [expression()],
    line :: line()
}).

-type function_name() :: binary().

-record(function, {
    name :: function_name() | undefined,
    args = [] :: [variable()],
    use = [] :: [variable()],
    code :: statements(),
    line :: line()
}).

-record(ref, {
    var :: variable(),
    line :: line()
}).

-record(concat, {
    texts :: [any()],
    line :: line()
}).

% variable values (ephp_vars)

-record(var_value, {
    content :: any()
}).

-record(var_ref, {
    pid :: reference() | undefined,
    ref :: #variable{} | undefined
}).

% classes

-record(class_const, {
    name :: binary(),
    value :: any()
}).

-type access_types() :: public | protected | private.

-record(class_attr, {
    name :: binary(),
    access = public :: access_types(),
    type = normal :: normal | static,
    init_value = null :: mixed()
}).

-type class_attr() :: #class_attr{}.

-record(class_method, {
    name :: binary(),
    args = [] :: [variable()],
    code :: [statement()],
    access = public :: access_types(),
    type = normal :: normal | static | abstract
}).

-type class_method() :: #class_method{}.
-type class_type() :: normal | static | abstract.

-record(class, {
    name :: class_name(),
    type = normal :: class_type(),
    extends :: undefined | class_name(),
    implements = [] :: [class_name()],
    constants = ?DICT:new() :: ?DICT_TYPE,
    attrs = [] :: [class_attr()],
    methods = [] :: [class_method()],
    line :: line(),
    instance_counter = 0 :: integer(),
    static_context :: context()
}).

-type class() :: #class{}.

-record(instance, {
    name :: class_name(),
    args :: [variable()],
    line :: line()
}).

-type instance() :: #instance{}.

-record(reg_func, {
    name :: binary(),
    args :: [variable()],
    type :: builtin | php,
    code = [] :: [statement()],
    builtin :: {Module :: atom(), Func :: atom()} | function(),
    pack_args = false :: boolean()
}).

-record(reg_instance, {
    id :: pos_integer(),
    class :: class(),
    instance :: instance(),
    context = ?DICT:new() :: ?DICT_TYPE
}).
