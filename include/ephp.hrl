-define(IS_DICT(D), (is_tuple(D) andalso element(1,D) =:= dict)).

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

-type index() :: arith_mono() | ternary() | binary() | operation().

-type post_decr() :: {post_decr, variable()}.
-type pre_decr() :: {pre_decr, variable()}.
-type post_incr() :: {post_incr, variable()}.
-type pre_incr() :: {pre_incr, variable()}.

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

-record(variable, {
    name :: binary(),
    idx = [] :: [index()]
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
