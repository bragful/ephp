

# Module ephp_parser #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_line-2">add_line/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_pos-2">add_pos/2</a></td><td></td></tr><tr><td valign="top"><a href="#arg_level-1">arg_level/1</a></td><td></td></tr><tr><td valign="top"><a href="#array_def_level-1">array_def_level/1</a></td><td></td></tr><tr><td valign="top"><a href="#code-3">code/3</a></td><td></td></tr><tr><td valign="top"><a href="#code_block-3">code_block/3</a></td><td></td></tr><tr><td valign="top"><a href="#code_block_level-1">code_block_level/1</a></td><td></td></tr><tr><td valign="top"><a href="#code_statement_level-1">code_statement_level/1</a></td><td></td></tr><tr><td valign="top"><a href="#comment_block-3">comment_block/3</a></td><td></td></tr><tr><td valign="top"><a href="#comment_line-3">comment_line/3</a></td><td></td></tr><tr><td valign="top"><a href="#constant-3">constant/3</a></td><td></td></tr><tr><td valign="top"><a href="#copy_level-2">copy_level/2</a></td><td></td></tr><tr><td valign="top"><a href="#enclosed_level-1">enclosed_level/1</a></td><td></td></tr><tr><td valign="top"><a href="#file-1">file/1</a></td><td></td></tr><tr><td valign="top"><a href="#new_line-1">new_line/1</a></td><td></td></tr><tr><td valign="top"><a href="#new_line-2">new_line/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#remove_spaces-2">remove_spaces/2</a></td><td></td></tr><tr><td valign="top"><a href="#throw_error-3">throw_error/3</a></td><td></td></tr><tr><td valign="top"><a href="#unclosed_level-1">unclosed_level/1</a></td><td></td></tr><tr><td valign="top"><a href="#variable-3">variable/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_line-2"></a>

### add_line/2 ###

`add_line(X, X2) -> any()`

<a name="add_pos-2"></a>

### add_pos/2 ###

`add_pos(X1, Offset) -> any()`

<a name="arg_level-1"></a>

### arg_level/1 ###

`arg_level(X1) -> any()`

<a name="array_def_level-1"></a>

### array_def_level/1 ###

`array_def_level(X1) -> any()`

<a name="code-3"></a>

### code/3 ###

`code(Rest, Pos, Parsed) -> any()`

<a name="code_block-3"></a>

### code_block/3 ###

`code_block(Rest, Pos, Parsed) -> any()`

<a name="code_block_level-1"></a>

### code_block_level/1 ###

`code_block_level(X1) -> any()`

<a name="code_statement_level-1"></a>

### code_statement_level/1 ###

`code_statement_level(X1) -> any()`

<a name="comment_block-3"></a>

### comment_block/3 ###

`comment_block(X1, Pos, Parsed) -> any()`

<a name="comment_line-3"></a>

### comment_line/3 ###

`comment_line(Rest, Pos, Parsed) -> any()`

<a name="constant-3"></a>

### constant/3 ###

`constant(Rest, Pos, Parsed) -> any()`

<a name="copy_level-2"></a>

### copy_level/2 ###

`copy_level(X1, X2) -> any()`

<a name="enclosed_level-1"></a>

### enclosed_level/1 ###

`enclosed_level(X1) -> any()`

<a name="file-1"></a>

### file/1 ###

`file(File) -> any()`

<a name="new_line-1"></a>

### new_line/1 ###

`new_line(Pos) -> any()`

<a name="new_line-2"></a>

### new_line/2 ###

`new_line(X1, N) -> any()`

<a name="parse-1"></a>

### parse/1 ###

`parse(Document) -> any()`

<a name="remove_spaces-2"></a>

### remove_spaces/2 ###

`remove_spaces(Rest, Pos) -> any()`

<a name="throw_error-3"></a>

### throw_error/3 ###

`throw_error(Error, X2, Data) -> any()`

<a name="unclosed_level-1"></a>

### unclosed_level/1 ###

`unclosed_level(X1) -> any()`

<a name="variable-3"></a>

### variable/3 ###

`variable(Rest, Pos, Var) -> any()`

