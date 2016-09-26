-module(ephp_datetime).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    get_abbr_weekday/1,
    get_abbr_month/1,
    get_timestamp/1,
    get_month/1,
    get_weekday/1,
    posix_time/0,
    timestamp/0,
    is_dst/2,
    to_zone/2,
    get_datetime/1,
    to_bmt/1,
    get_tz_time/3,
    tz/1
]).

-spec get_timestamp(TS::integer() | float()) -> timer:timestamp().

get_timestamp(Timestamp) ->
    M = trunc(Timestamp) div 1000000,
    S = trunc(Timestamp) rem 1000000,
    U = trunc(Timestamp * 1000000) rem 1000000,
    {M,S,U}.

-spec get_abbr_month(calendar:month()) -> binary().

get_abbr_month(1) -> <<"Jan">>;
get_abbr_month(2) -> <<"Feb">>;
get_abbr_month(3) -> <<"Mar">>;
get_abbr_month(4) -> <<"Apr">>;
get_abbr_month(5) -> <<"May">>;
get_abbr_month(6) -> <<"Jun">>;
get_abbr_month(7) -> <<"Jul">>;
get_abbr_month(8) -> <<"Aug">>;
get_abbr_month(9) -> <<"Sep">>;
get_abbr_month(10) -> <<"Oct">>;
get_abbr_month(11) -> <<"Nov">>;
get_abbr_month(12) -> <<"Dec">>.

-spec get_month(calendar:month()) -> binary().

get_month(1) -> <<"January">>;
get_month(2) -> <<"February">>;
get_month(3) -> <<"March">>;
get_month(4) -> <<"April">>;
get_month(5) -> <<"May">>;
get_month(6) -> <<"June">>;
get_month(7) -> <<"July">>;
get_month(8) -> <<"August">>;
get_month(9) -> <<"September">>;
get_month(10) -> <<"October">>;
get_month(11) -> <<"November">>;
get_month(12) -> <<"December">>.

-spec get_abbr_weekday(calendar:daynum()) -> binary().

get_abbr_weekday(1) -> <<"Mon">>;
get_abbr_weekday(2) -> <<"Tue">>;
get_abbr_weekday(3) -> <<"Wed">>;
get_abbr_weekday(4) -> <<"Thu">>;
get_abbr_weekday(5) -> <<"Fri">>;
get_abbr_weekday(6) -> <<"Sat">>;
get_abbr_weekday(7) -> <<"Sun">>.

-spec get_weekday(calendar:daynum()) -> binary().

get_weekday(1) -> <<"Monday">>;
get_weekday(2) -> <<"Tuesday">>;
get_weekday(3) -> <<"Wednesday">>;
get_weekday(4) -> <<"Thursday">>;
get_weekday(5) -> <<"Friday">>;
get_weekday(6) -> <<"Saturday">>;
get_weekday(7) -> <<"Sunday">>.

-spec posix_time() -> integer().

posix_time() ->
    {MS,S,_} = timestamp(),
    MS * 1000000 + S.

-spec timestamp() -> os:timestamp().

-ifdef(TEST).
timestamp() ->
    {1474,806235,701464}.
-else.
timestamp() ->
    os:timestamp().
-endif.

-spec normalize_tz(string() | binary()) -> string().

normalize_tz(TZ) when is_binary(TZ) ->
    normalize_tz(binary_to_list(TZ));
normalize_tz("UTC") ->
    "Etc/UTC";
normalize_tz("GMT") ->
    "Etc/GMT";
%% TODO: normalize more timezones and throw error...
normalize_tz(Zone) ->
    Zone.

-spec tz(string() | binary()) -> binary().

tz(TZ) when is_list(TZ) ->
    tz(list_to_binary(TZ));
tz(<<"Etc/UTC">>) ->
    <<"UTC">>;
tz(<<"Etc/GMT">>) ->
    <<"GMT">>;
tz(Other) ->
    Other.

-spec is_dst(calendar:date(), string() | binary()) -> boolean().

is_dst(Date, RawTZ) ->
    TZ = normalize_tz(RawTZ),
    ezic:has_dst_local(Date, TZ).

-spec to_zone(calendar:datetime(), string() | binary()) -> calendar:datetime().

to_zone(Date, RawTZ) ->
    TZ = normalize_tz(RawTZ),
    ezic:utc_to_local(calendar:now_to_universal_time(Date), TZ).

-spec get_datetime(pos_integer()) -> calendar:datetime().

get_datetime(Timestamp) when is_number(Timestamp) ->
    {_,_,_} = T = get_timestamp(Timestamp),
    calendar:now_to_datetime(T).

-spec to_bmt(calendar:datetime()) -> pos_integer().

to_bmt(DateTime) ->
    {_,{H,M,S}} = ezic:utc_to_local(DateTime, "Etc/GMT-1"),
    trunc(((((H * 60) + M) * 60) + S) / 86.4).

-spec get_tz_time(calendar:datetime(), string() | binary(), binary()) -> binary().

get_tz_time(DateTime, TZ, Sep) ->
    UTCDateTime = ezic:local_to_utc(DateTime, normalize_tz(TZ)),
    UTCTime = calendar:datetime_to_gregorian_seconds(UTCDateTime),
    Time = calendar:datetime_to_gregorian_seconds(DateTime),
    case UTCTime - Time of
        Diff when Diff =< 0 ->
            <<"+",
              (ephp_util:pad_to_bin(abs(Diff div 3600),2))/binary, Sep/binary,
              (ephp_util:pad_to_bin(abs(Diff rem 3600),2))/binary>>;
        Diff when Diff > 0 ->
            <<"-",
              (ephp_util:pad_to_bin(Diff div 3600, 2))/binary, Sep/binary,
              (ephp_util:pad_to_bin(Diff rem 3600, 2))/binary>>
    end.
