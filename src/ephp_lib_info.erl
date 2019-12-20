-module(ephp_lib_info).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include_lib("stdlib/include/zip.hrl").
-include("ephp.hrl").

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    phpinfo/3,
    phpversion/2,
    php_sapi_name/2,
    php_logo_guid/2,
    ini_get/3,
    ini_set/4,
    set_include_path/3,
    get_include_path/2,
    version_compare/5,
    extension_loaded/3,
    memory_get_usage/3,
    memory_get_peak_usage/3
]).

-define(INFO_GENERAL,       2#0000001).
-define(INFO_CREDITS,       2#0000010).
-define(INFO_CONFIGURATION, 2#0000100).
-define(INFO_MODULES,       2#0001000).
-define(INFO_ENVIRONMENT,   2#0010000).
-define(INFO_VARIABLES,     2#0100000).
-define(INFO_LICENSE,       2#1000000).
-define(INFO_ALL,           2#1111111).

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {phpinfo, [
        {args, {0, 1, undefined, [{integer, ?INFO_ALL}]}}
    ]},
    php_sapi_name,
    php_logo_guid,
    phpversion,
    ini_get,
    ini_set,
    set_include_path,
    get_include_path,
    {version_compare, [
        {args, {2, 2, undefined, [string, string, {string, undefined}]}}
    ]},
    {extension_loaded, [
        {args, {1, 1, undefined, [string]}}
    ]},
    {memory_get_usage, [
        {args, {0, 1, undefined, [{boolean, false}]}}
    ]},
    {memory_get_peak_usage, [
        {args, {0, 1, undefined, [{boolean, false}]}}
    ]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [
    {<<"PHP_VERSION">>, ?PHP_VERSION},
    {<<"PHP_MAJOR_VERSION">>, <<?PHP_MAJOR_VERSION>>},
    {<<"PHP_MINOR_VERSION">>, <<?PHP_MINOR_VERSION>>},
    {<<"PHP_RELEASE_VERSION">>, <<?PHP_RELEASE_VERSION>>},
    {<<"PHP_VERSION_ID">>, ?PHP_VERSION_ID},
    {<<"PHP_EXTRA_VERSION">>, ?PHP_EXTRA_VERSION},
    % Zend Thread Safety
    {<<"PHP_ZTS">>, 0},
    {<<"PHP_DEBUG">>, 0},
    % TODO: move to ephp_lib_file?
    {<<"PHP_MAXPATHLEN">>, 1024},
    {<<"PHP_OS">>, get_os()},
    {<<"PHP_OS_FAMILY">>, get_os_family()},
    {<<"PHP_SAPI">>, ephp_config:get(sapi_type, <<"cli">>)},
    {<<"PHP_EOL">>, get_os_eol()},
    % fake:
    {<<"PHP_SHLIB_SUFFIX">>, <<"so">>},
    % assert options:
    {<<"INI_USER">>, 1},
    {<<"INI_PERDIR">>, 2},
    {<<"INI_SYSTEM">>, 4},
    {<<"INI_ALL">>, 7},
    % phpinfo constants:
    {<<"INFO_GENERAL">>, ?INFO_GENERAL},
    {<<"INFO_CREDITS">>, ?INFO_CREDITS},
    {<<"INFO_CONFIGURATION">>, ?INFO_CONFIGURATION},
    {<<"INFO_MODULES">>, ?INFO_MODULES},
    {<<"INFO_ENVIRONMENT">>, ?INFO_ENVIRONMENT},
    {<<"INFO_VARIABLES">>, ?INFO_VARIABLES},
    {<<"INFO_LICENSE">>, ?INFO_LICENSE},
    {<<"INFO_ALL">>, ?INFO_ALL}
].

-spec phpinfo(context(), line(), var_value()) -> undefined.

phpinfo(Context, Line, {_, Flags}) ->
    case php_sapi_name(Context, Line) of
        <<"cli", _/binary>> -> process_phpinfo("text", Context, Flags);
        _ -> process_phpinfo("html", Context, Flags)
    end.

-spec phpversion(context(), line()) -> binary().

phpversion(_Context, _Line) ->
    ?PHP_VERSION.

-spec php_sapi_name(context(), line()) -> binary().

php_sapi_name(_Context, _Line) ->
    ephp_config:get(sapi_type, <<"cli">>).

-spec php_logo_guid(context(), line()) -> binary().

php_logo_guid(_Context, _Line) ->
    Content = get_file("bragful_logo.png"),
    Base64Content = base64:encode(Content),
    <<"data:image/png;base64,", Base64Content/binary>>.

-spec ini_get(context(), line(), var_value()) -> mixed().

ini_get(_Context, _Line, {_,Key}) ->
    ephp_config:get(Key).

-spec ini_set(context(), line(), var_value(), var_value()) -> binary().

ini_set(_Context, _Line, {_,Key}, {_,Value}) ->
    PrevVal = ephp_config:get(Key),
    ephp_config:set(Key, Value),
    PrevVal.

-spec set_include_path(context(), line(), var_value()) -> binary().

set_include_path(_Context, _Line, {_,NewPath}) ->
    ephp_config:set(<<"include_path">>, NewPath),
    NewPath.

-spec get_include_path(context(), line()) -> binary().

get_include_path(_Context, _Line) ->
    ephp_config:get(<<"include_path">>).

-spec version_compare(context(), line(),
                      Vsn1::var_value(),
                      Vsn2::var_value(),
                      Op::var_value()) -> boolean() | integer().

version_compare(_Context, _Line, {_, Vsn1}, {_, Vsn2}, {_, undefined}) ->
    ephp_string:vsn_cmp(Vsn1, Vsn2);

version_compare(_Context, _Line, {_, Vsn1}, {_, Vsn2}, {_, Op}) ->
    case {Op, ephp_string:vsn_cmp(Vsn1, Vsn2)} of
        {<<"<">>, -1} -> true;
        {<<"lt">>, -1} -> true;
        {<<"<=">>, I} when I =< 0 -> true;
        {<<"le">>, I} when I =< 0 -> true;
        {<<">">>, 1} -> true;
        {<<"gt">>, 1} -> true;
        {<<">=">>, I} when I >= 0 -> true;
        {<<"ge">>, I} when I >= 0 -> true;
        {<<"==">>, 0} -> true;
        {<<"=">>, 0} -> true;
        {<<"eq">>, 0} -> true;
        {<<"!=">>, I} when I =/= 0 -> true;
        {<<"<>">>, I} when I =/= 0 -> true;
        {<<"ne">>, I} when I =/= 0 -> true;
        _ -> false
    end.

-spec extension_loaded(context(), line(), Name :: var_value()) -> boolean().

extension_loaded(_Context, _Line, {_, ModuleName}) ->
    Modules = lists:map(fun(M) ->
        case atom_to_binary(M, utf8) of
            <<"ephp_lib_", Module/binary>> -> Module;
            Module -> Module
        end
    end, ephp_config:get(modules, [])),
    lists:member(ModuleName, Modules).

-spec memory_get_usage(context(), line(), RealUsage :: var_value()) ->
      pos_integer().

memory_get_usage(_Context, _Line, {_, false}) ->
    recon_alloc:memory(used, current);
memory_get_usage(_Context, _Line, {_, true}) ->
    recon_alloc:memory(allocated, current).

-spec memory_get_peak_usage(context(), line(), RealUsage :: var_value()) ->
      pos_integer().

memory_get_peak_usage(_Context, _Line, {_, false}) ->
    recon_alloc:memory(used, max);
memory_get_peak_usage(_Context, _Line, {_, true}) ->
    recon_alloc:memory(allocated, max).

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

process_directives(Values) ->
    D = lists:foldl(fun process_directives/2, orddict:new(), Values),
    ephp_array:from_list([ {K, ephp_array:from_list(V)} || {K, V} <- D ]).

process_directives({K, V}, Dict) ->
    case binary:split(K, <<".">>) of
        [K] -> safe_append(<<"core">>, {K, V}, Dict);
        [Section, _] -> safe_append(Section, {K, V}, Dict)
    end.

safe_append(K, V, Dict) ->
    case orddict:is_key(K, Dict) of
        true -> orddict:append(K, V, Dict);
        false -> orddict:store(K, [V], Dict)
    end.

phpinfo_metadata() ->
    OtpRelease = list_to_binary(erlang:system_info(otp_release)),
    IniFile = filename:dirname(?PHP_INI_FILE),
    Directives = [ Cfg || {K, _} = Cfg <- application:get_all_env(ephp),
                   is_binary(K) or is_integer(K) ],
    ArrDirectives = process_directives(Directives),
    Streams = ephp_array:from_list(ephp_stream:list_streams()),
    ephp_array:from_list([{<<"version">>, ?PHP_VERSION},
                          {<<"vsn">>, get_vsn()},
                          {<<"build_date">>, get_build_date()},
                          {<<"release">>, OtpRelease},
                          {<<"path_php_ini">>, IniFile},
                          {<<"directives">>, ArrDirectives},
                          {<<"streams">>, Streams},
                          {<<"tz_version">>, ephp_datetime:get_tz_version()}]).

process_phpinfo(Format, Context, Flags) ->
    Metadata = phpinfo_metadata(),
    PHPInfo = get_file("phpinfo." ++ Format ++ ".php"),
    ephp:register_var(Context, <<"_METADATA">>, Metadata),
    ephp:register_var(Context, <<"_FLAGS">>, Flags),
    ephp:register_var(Context, <<"LICENSE">>, get_file("license")),
    {ok, false} = ephp:eval(Context, PHPInfo),
    undefined.

get_file(Name, Filename) when Name =:= "rebar3" orelse Name =:= undefined ->
    FullFilename = filename:join(code:priv_dir(ephp), Filename),
    {ok, Content} = file:read_file(FullFilename),
    Content;

get_file(Name, Filename) ->
    {ok, Sections} = escript:extract(Name, []),
    Zip = proplists:get_value(archive, Sections),
    Filter = fun(#zip_file{name = "ephp/priv/" ++ _}) -> true;
                (_) -> false
                end,
    {ok, Files} = zip:extract(Zip, [{file_filter, Filter}, memory]),
    NewFilename = "ephp/priv/" ++ Filename,
    proplists:get_value(NewFilename, Files).

get_file(Filename) ->
    try
        Name = filename:basename(escript:script_name()),
        get_file(Name, Filename)
    catch
        error:{badmatch, []} -> get_file(undefined, Filename)
    end.

-spec get_vsn() -> binary().

get_vsn() ->
    {ok, Vsn} = application:get_key(ephp, vsn),
    list_to_binary(Vsn).

-spec get_build_date() -> binary().

-ifndef(BUILD_DATE).
%% keep it only for some editors (like VSCode)
-define(BUILD_DATE, "Aug 26 2019 13:57:10").
-endif.

get_build_date() ->
    list_to_binary(?BUILD_DATE).

-spec get_os() -> binary().

get_os() ->
    %% TODO complete the list of OS
    case os:type() of
        {unix, darwin} -> <<"Darwin">>;
        {unix, linux} -> <<"Linux">>;
        {unix, freebsd} -> <<"FreeBSD">>;
        {win32, nt} -> <<"WINNT">>;
        {win32, _} -> <<"Windows">>
    end.

-spec get_os_family() -> binary().

get_os_family() ->
    %% TODO complete the list of OS families
    case os:type() of
        {unix, darwin} -> <<"OSX">>;
        {unix, linux} -> <<"LINUX">>;
        %% TODO Add more BSD
        {unix, freebsd} -> <<"BSD">>;
        %% TODO Add Solaris
        {win32, _} -> <<"WIN">>;
        {_, _} -> <<"unknown">>
    end.

-spec get_os_eol() -> binary().

get_os_eol() ->
    %% TODO change this for something more coherent :-P
    <<"\n">>.
