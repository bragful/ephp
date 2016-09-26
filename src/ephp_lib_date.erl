-module(ephp_lib_date).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    time/2,
    microtime/2,
    microtime/3,
    date/3,
    date/4,
    gmdate/3,
    gmdate/4,
    date_default_timezone_set/3,
    date_default_timezone_get/2
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    time, date, gmdate, microtime,
    date_default_timezone_get,
    date_default_timezone_set
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec time(context(), line()) -> integer().

time(_Context, _Line) ->
    ephp_datetime:posix_time().

-spec microtime(context(), line(), GetAsFloat :: {variable(),boolean()}) ->
    float() | binary().

microtime(_Context, _Line, {_, true}) ->
    {MS,S,US} = ephp_datetime:timestamp(),
    MS * 1000000 + S + ((US div 1000) / 1000);
microtime(_Context, _Line, {_, false}) ->
    {MS,S,US} = ephp_datetime:timestamp(),
    Posix = ephp_util:to_bin(MS * 1000000 + S),
    MiliSec = ephp_util:to_bin((US div 1000) / 1000),
    <<MiliSec/binary, " ", Posix/binary>>.

-spec microtime(context(), line()) -> binary().

microtime(Context, Line) ->
    microtime(Context, Line, {undefined, false}).

-spec date(context(), line(), Format :: {variable(),binary()}) -> binary().

date(Context, Line, {_,Format}) ->
    {MS,S,US} = ephp_datetime:timestamp(),
    date(Context, Line, {"", Format}, {"", (MS * 1000000) + S + (US / 1000000)}).

-spec date(
    context(), line(),
    Format :: {variable(),binary()},
    Timestamp :: {variable(),(integer() | float())}) -> binary().

date(Context, _Line, {_,Format}, {_,Timestamp}) ->
    {M,S,U} = ephp_datetime:get_timestamp(Timestamp),
    TZ = ephp_context:get_tz(Context),
    Date = ephp_datetime:to_zone({M,S,U}, TZ),
    date_format(Format, <<>>, {Timestamp, Date, TZ}).

-spec gmdate(context(), line(), Format :: {variable(),binary()}) -> binary().

gmdate(Context, Line, {_,Format}) ->
    {MS,S,US} = ephp_datetime:timestamp(),
    gmdate(Context, Line, Format, (MS * 1000000) + S + (US / 1000000)).

-spec gmdate(
    context(), line(),
    Format :: {variable(),binary()},
    Timestamp :: integer() | float()) -> binary().

gmdate(_Context, _Line, Format, Timestamp) ->
    {M,S,U} = ephp_datetime:get_timestamp(Timestamp),
    TZ = "GMT",
    Date = calendar:now_to_universal_time({M,S,U}),
    date_format(Format, <<>>, {Timestamp, Date, TZ}).

-spec date_default_timezone_get(context(), line()) -> binary().

date_default_timezone_get(Context, _Line) ->
    ephp_util:to_bin(ephp_context:get_tz(Context)).

-spec date_default_timezone_set(
    context(), line(),
    TZ :: {variable(),binary()}) -> binary().

date_default_timezone_set(Context, _Line, {_,TZ}) ->
    ephp_context:set_tz(Context, TZ),
    undefined.

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

-spec date_format(ToAnalyze :: binary(), Result :: binary(),
    {Timestamp :: integer(), Date :: date()}) -> binary().

date_format(<<>>, Result, _Date) ->
    Result;

date_format(<<"d",R/binary>>, Result, {_,{{_,_,D},_},_}=Date) ->
    Day = ephp_util:pad_to_bin(D, 2),
    date_format(R,<<Result/binary, Day/binary>>, Date);

date_format(<<"D",R/binary>>, Result, {_,{D,_},_}=Date) ->
    Day = ephp_datetime:get_abbr_weekday(calendar:day_of_the_week(D)),
    date_format(R,<<Result/binary, Day/binary>>, Date);

date_format(<<"j",R/binary>>, Result, {_,{{_,_,D},_},_}=Date) ->
    Month = ephp_util:to_bin(D),
    date_format(R,<<Result/binary, Month/binary>>, Date);

date_format(<<"l",R/binary>>, Result, {_,{D,_},_}=Date) ->
    Day = ephp_datetime:get_weekday(calendar:day_of_the_week(D)),
    date_format(R,<<Result/binary, Day/binary>>, Date);

date_format(<<"N",R/binary>>, Result, {_,{D,_},_}=Date) ->
    Day = calendar:day_of_the_week(D),
    DayBin = ephp_util:to_bin(Day),
    date_format(R,<<Result/binary, DayBin/binary>>, Date);

date_format(<<"S",R/binary>>, Result, {_,{{_,_,D},_},_}=Date) ->
    Suffix = if
        D =:= 1 orelse D =:= 21 orelse D =:= 31 -> <<"st">>;
        D =:= 2 orelse D =:= 22 -> <<"nd">>;
        D =:= 3 orelse D =:= 23 -> <<"rd">>;
        true -> <<"th">>
    end,
    date_format(R,<<Result/binary, Suffix/binary>>, Date);

date_format(<<"w",R/binary>>, Result, {_,{D,_},_}=Date) ->
    Day = calendar:day_of_the_week(D),
    DayBin = ephp_util:to_bin(if
        Day =:= 7 -> 0;
        true -> Day
    end),
    date_format(R,<<Result/binary, DayBin/binary>>, Date);

date_format(<<"z",R/binary>>, Result, {_,{{Y,M,D},_},_}=Date) ->
    Days1 = calendar:date_to_gregorian_days(Y,1,1),
    Days2 = calendar:date_to_gregorian_days(Y,M,D),
    Days = ephp_util:to_bin(Days2 - Days1),
    date_format(R,<<Result/binary, Days/binary>>, Date);

date_format(<<"W",R/binary>>, Result, {_,{D,_},_}=Date) ->
    {_Y,WY} = calendar:iso_week_number(D),
    WeekOfYear = ephp_util:pad_to_bin(WY, 2),
    date_format(R,<<Result/binary, WeekOfYear/binary>>, Date);

date_format(<<"F",R/binary>>, Result, {_,{{_,M,_},_},_}=Date) ->
    Month = ephp_datetime:get_month(M),
    date_format(R,<<Result/binary, Month/binary>>, Date);

date_format(<<"m",R/binary>>, Result, {_,{{_,M,_},_},_}=Date) ->
    Month = ephp_util:pad_to_bin(M, 2),
    date_format(R,<<Result/binary, Month/binary>>, Date);

date_format(<<"M",R/binary>>, Result, {_,{{_,M,_},_},_}=Date) ->
    Month = ephp_datetime:get_abbr_month(M),
    date_format(R,<<Result/binary, Month/binary>>, Date);

date_format(<<"n",R/binary>>, Result, {_,{{_,M,_},_},_}=Date) ->
    Month = ephp_util:to_bin(M),
    date_format(R,<<Result/binary, Month/binary>>, Date);

date_format(<<"t",R/binary>>, Result, {_,{{Y,M,_},_},_}=Date) ->
    Month = ephp_util:to_bin(calendar:last_day_of_the_month(Y,M)),
    date_format(R,<<Result/binary, Month/binary>>, Date);

date_format(<<"L",R/binary>>, Result, {_,{{Y,_,_},_},_}=Date) ->
    Leap = case calendar:is_leap_year(Y) of
        true -> <<"1">>;
        false -> <<"0">>
    end,
    date_format(R,<<Result/binary, Leap/binary>>, Date);

date_format(<<"o",R/binary>>, Result, Date) ->
    %% TODO: ISO-8601 year number. This has the same value as Y,
    %% except that if the ISO week number (W) belongs to the previous
    %% or next year, that year is used instead.
    Month = <<"o">>,
    date_format(R,<<Result/binary, Month/binary>>, Date);

date_format(<<"Y",R/binary>>, Result, {_,{{Y,_,_},_},_}=Date) ->
    Year = ephp_util:to_bin(Y),
    date_format(R,<<Result/binary, Year/binary>>, Date);

date_format(<<"y",R/binary>>, Result, {_,{{Y,_,_},_},_}=Date) ->
    Year = ephp_util:pad_to_bin(Y rem 100, 2),
    date_format(R,<<Result/binary, Year/binary>>, Date);

date_format(<<"a",R/binary>>, Result, {_,{_,{H,_,_}},_}=Date) ->
    Type = if
        H >= 0 andalso H =< 11 -> <<"am">>;
        true -> <<"pm">>
    end,
    date_format(R,<<Result/binary, Type/binary>>, Date);

date_format(<<"A",R/binary>>, Result, {_,{_,{H,_,_}},_}=Date) ->
    Type = if
        H >= 0 andalso H =< 11 -> <<"AM">>;
        true -> <<"PM">>
    end,
    date_format(R,<<Result/binary, Type/binary>>, Date);

date_format(<<"B",R/binary>>, Result, {Timestamp,_,_}=Date) ->
    DateTime = ephp_datetime:get_datetime(Timestamp),
    BeatsTime = ephp_datetime:to_bmt(DateTime),
    Beats = ephp_util:to_bin(BeatsTime),
    date_format(R,<<Result/binary, Beats/binary>>, Date);

date_format(<<"g",R/binary>>, Result, {_,{_,{H,_,_}},_}=Date) ->
    Hour = ephp_util:to_bin((H + 11) rem 12 + 1),
    date_format(R, <<Result/binary,Hour/binary>>, Date);

date_format(<<"G",R/binary>>, Result, {_,{_,{H,_,_}},_}=Date) ->
    Hour = ephp_util:to_bin(H),
    date_format(R, <<Result/binary,Hour/binary>>, Date);

date_format(<<"h",R/binary>>, Result, {_,{_,{H,_,_}},_}=Date) ->
    Hour = ephp_util:pad_to_bin((H + 11) rem 12 + 1, 2),
    date_format(R, <<Result/binary,Hour/binary>>, Date);

date_format(<<"H",R/binary>>, Result, {_,{_,{H,_,_}},_}=Date) ->
    Hour = ephp_util:pad_to_bin(H, 2),
    date_format(R, <<Result/binary,Hour/binary>>, Date);

date_format(<<"i",R/binary>>, Result, {_,{_,{_,M,_}},_}=Date) ->
    Minute = ephp_util:pad_to_bin(M, 2),
    date_format(R, <<Result/binary,Minute/binary>>, Date);

date_format(<<"s",R/binary>>, Result, {_,{_,{_,_,S}},_}=Date) ->
    Second = ephp_util:pad_to_bin(S, 2),
    date_format(R, <<Result/binary,Second/binary>>, Date);

date_format(<<"u",R/binary>>, Result, {Timestamp,_,_}=Date) ->
    US = (Timestamp * 1000000) rem 1000000,
    MSecond = ephp_util:pad_to_bin(US, 6),
    date_format(R, <<Result/binary,MSecond/binary>>, Date);

date_format(<<"e",R/binary>>, Result, {_,_,TZ}=Date) ->
    TimeZone = ephp_util:to_bin(TZ),
    date_format(R, <<Result/binary,TimeZone/binary>>, Date);

date_format(<<"I",R/binary>>, Result, {_,D,TZ}=Date) ->
    Daylight = case ephp_datetime:is_dst(D, TZ) of
        true -> <<"1">>;
        false -> <<"0">>
    end,
    date_format(R, <<Result/binary,Daylight/binary>>, Date);

date_format(<<"O",R/binary>>, Result, {_,DT,TZ}=Date) ->
    Timezone = ephp_datetime:get_tz_time(DT, TZ, <<>>),
    date_format(R, <<Result/binary,Timezone/binary>>, Date);

date_format(<<"P",R/binary>>, Result, {_,DT,TZ}=Date) ->
    Timezone = ephp_datetime:get_tz_time(DT, TZ, <<":">>),
    date_format(R, <<Result/binary,Timezone/binary>>, Date);

date_format(<<"T",R/binary>>, Result, {_,_,TZ}=Date) ->
    %% TODO translate timezones like Europe/Madrid to CET and CEST
    Timezone = ephp_datetime:tz(TZ),
    date_format(R, <<Result/binary,Timezone/binary>>, Date);

date_format(<<"Z",R/binary>>, Result, {_,DT,TZ}=Date) ->
    <<S:1/binary,H:2/binary,M:2/binary>> = ephp_datetime:get_tz_time(DT,TZ,<<>>),
    Secs = integer_to_binary((binary_to_integer(H) * 60 + binary_to_integer(M)) * 60),
    Timezone = case S of
        <<"+">> -> Secs;
        <<"-">> -> <<S/binary, Secs/binary>>
    end,
    date_format(R, <<Result/binary,Timezone/binary>>, Date);

date_format(<<"c",R/binary>>, Result, {_,{{Y,M,D},{H,I,S}}=DT,TZ}=Date) ->
    YB = ephp_util:to_bin(Y),
    MB = ephp_util:pad_to_bin(M, 2),
    DB = ephp_util:pad_to_bin(D, 2),
    HB = ephp_util:pad_to_bin(H, 2),
    IB = ephp_util:pad_to_bin(I, 2),
    SB = ephp_util:pad_to_bin(S, 2),
    TZtime = ephp_datetime:get_tz_time(DT, TZ, <<":">>),
    DateTime = <<YB/binary,"-",MB/binary,"-",DB/binary,"T",
        HB/binary,":",IB/binary,":",SB/binary,TZtime/binary>>,
    date_format(R, <<Result/binary,DateTime/binary>>, Date);

date_format(<<"r",R/binary>>, Result, {_,{{Y,M,D}=Dt,{H,I,S}}=DT,TZ}=Date) ->
    WB = ephp_datetime:get_abbr_weekday(calendar:day_of_the_week(Dt)),
    DB = ephp_util:pad_to_bin(D, 2),
    MB = ephp_datetime:get_abbr_month(M),
    YB = ephp_util:to_bin(Y),
    HB = ephp_util:pad_to_bin(H, 2),
    IB = ephp_util:pad_to_bin(I, 2),
    SB = ephp_util:pad_to_bin(S, 2),
    TZtime = ephp_datetime:get_tz_time(DT, TZ, <<>>),
    DateTime = <<WB/binary,", ",
        DB/binary," ",MB/binary," ",YB/binary," ",
        HB/binary,":",IB/binary,":",SB/binary," ",
        TZtime/binary>>,
    date_format(R, <<Result/binary,DateTime/binary>>, Date);

date_format(<<"U",R/binary>>, Result, {Timestamp,_,_}=Date) ->
    DateTime = ephp_util:to_bin(trunc(Timestamp)),
    date_format(R, <<Result/binary,DateTime/binary>>, Date);

date_format(<<Other:1/binary,R/binary>>, Result, Date) ->
    date_format(R, <<Result/binary,Other/binary>>, Date).
