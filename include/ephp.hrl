
% -define(DICT, dict).
% -define(IS_DICT(D), (is_tuple(D) andalso element(1,D) =:= dict)).

-define(DICT, orddict).
%% FIXME: do a better way to do this:
-define(IS_DICT(D), (is_list(D))).

-define(SETS, sets).

% built-in modules
-define(MODULES, [
    ephp_func_date,
    ephp_func_vars,
    ephp_func_misc,
    ephp_func_ob,
    ephp_func_control,
    ephp_func_array,
    ephp_func_string
]).


-type mixed() :: 
    integer() | float() | binary() | boolean() | null.

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

-record(constant, {
    name :: binary(),
    line :: line()
}).

-type constant() :: #constant{}.

-record(variable, {
    name :: binary(),
    idx = [] :: [array_index()],
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

-record(call, {
    name :: binary(),
    args = [] :: [expression()],
    line :: line()
}).

-record(function, {
    name :: binary(),
    args = [] :: [variable()],
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
