
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

-type context() :: pid().

-type statement() :: tuple() | atom().

-type expression() :: operation().

-type reason() :: atom() | string().

% main statements

-record(eval, {
    statements :: [statement()]
}).

-record(print, {
    expression :: expression()
}).

-record(print_text, {
    text :: binary()
}).

-type main_statement() :: #eval{} | #print{} | #print_text{}.

% blocks

-record(if_block, {
    conditions :: [condition()],
    true_block :: [statement()],
    false_block :: [statement()]
}).

-record(for, {
    init :: expression(),
    conditions :: [condition()],
    update :: expression(),
    loop_block :: [statement()]
}).

-record(while, {
    type :: (pre | post),
    conditions :: [condition()],
    loop_block :: [statement()]
}).

-record(foreach, {
    kiter :: variable(),
    iter :: variable(),
    elements :: variable(),
    loop_block :: [statement()]
}).

-type if_block() :: #if_block{}.

% data types and operations

-type ternary() :: if_block().

-record(operation, {
    type :: binary() | atom(),
    expression_left :: variable(),
    expression_right :: expression()
}).

-type operation_not() :: {operation_not, condition()}.

-type condition() :: expression() | operation().

-type operation() :: #operation{}.

-type arith_mono() :: pre_incr() | pre_decr() | post_incr() | post_decr().

-type array_index() :: arith_mono() | ternary() | binary() | operation().

-type post_decr() :: {post_decr, variable()}.
-type pre_decr() :: {pre_decr, variable()}.
-type post_incr() :: {post_incr, variable()}.
-type pre_incr() :: {pre_incr, variable()}.

-type return() :: {return, expression()}.

-record(int, {
    int :: integer()
}).

-record(float, {
    float :: float()
}).

-record(text, {
    text :: binary()
}).

-record(text_to_process, {
    text :: [expression() | variable() | binary()]
}).

-record(constant, {
    name :: binary()
}).

-type constant() :: #constant{}.

-record(variable, {
    name :: binary(),
    idx = [] :: [array_index()]
}).

-type variable() :: #variable{}.

-record(array_element, {
    idx = auto :: auto | expression(),
    element :: expression()
}).

-type array_element() :: #array_element{}.

-record(array, {
    elements = [] :: [array_element()]
}).

-type php_array() :: #array{}.

% statements

-record(assign, {
    variable :: variable(),
    expression :: expression()
}).

-record(call, {
    name :: binary(),
    args = [] :: [expression()]
}).

-record(function, {
    name :: binary(),
    args = [] :: [variable()],
    code :: [statement()]
}).

% variable values (ephp_vars)

-record(var_value, {
    content :: any()
}).

-record(var_ref, {
    pid :: pid() | undefined,
    ref :: #variable{} | undefined
}).
