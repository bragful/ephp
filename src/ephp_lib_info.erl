-module(ephp_lib_info).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    phpinfo/2,
    phpversion/2,
    ini_get/3,
    set_include_path/3
]).

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    phpinfo,
    phpversion,
    ini_get,
    set_include_path
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

-spec phpinfo(context(), line()) -> undefined.

phpinfo(Context, _Line) ->
    Version = ?PHP_VERSION,
    Vsn = get_vsn(),
    BuildDate = get_build_date(),
    Release = list_to_binary(erlang:system_info(otp_release)),
    PathPHPini = filename:dirname(?PHP_INI_FILE),
    Directives = lists:foldl(fun
        ({_K,<<>>},R) -> R;
        ({K,V},R) when is_binary(K) ->
            <<R/binary, K/binary, " => ", (ephp_data:to_bin(V))/binary, "\n">>;
        (_,R) -> R
    end, <<>>, application:get_all_env(ephp)),
    Output = <<
    "phpinfo()\n"
    "PHP Version => ", Version/binary, "\n"
    "\n"
    "System => Erlang/OTP ", Release/binary, " ephp ", Vsn/binary, "\n"
    "Build Date => ", BuildDate/binary, "\n"
    "Configuration File (php.ini) Path => ", PathPHPini/binary, "\n"
    "\n"
    "Core\n"
    "\n"
    "PHP Version => ", Version/binary, "\n"
    "\n"
    "Directive => Value\n",
    Directives/binary,
    "\n"
    >>,
    ephp_context:set_output(Context, Output),
    undefined.

-spec phpversion(context(), line()) -> binary().

phpversion(_Context, _Line) ->
    ?PHP_VERSION.

-spec ini_get(context(), line(), var_value()) -> mixed().

ini_get(_Context, _Line, {_,Key}) ->
    ephp_config:get(Key).

-spec set_include_path(context(), line(), var_value()) -> binary().

set_include_path(_Context, _Line, {_,NewPath}) ->
    ephp_config:set(<<"include_path">>, NewPath),
    NewPath.

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

-spec get_vsn() -> binary().

get_vsn() ->
    {ok, Vsn} = application:get_key(ephp, vsn),
    list_to_binary(Vsn).

-spec get_build_date() -> binary().

get_build_date() ->
    Info = ephp:module_info(),
    Compile = proplists:get_value(compile, Info),
    {Y,M,D,H,I,S} = proplists:get_value(time, Compile),
    Month = ephp_datetime:get_abbr_month(M),
    Day = ephp_data:pad_to_bin(D,2),
    Year = integer_to_binary(Y),
    Hour = ephp_data:pad_to_bin(H,2),
    Min = ephp_data:pad_to_bin(I,2),
    Sec = ephp_data:pad_to_bin(S,2),
    <<
        Month/binary, " ", Day/binary, " ", Year/binary, " ",
        Hour/binary, ":", Min/binary, ":", Sec/binary
    >>.
