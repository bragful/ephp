-module(ephp_func_date).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init/0,
    time/2,
    date/3,
    date/4,
    gmdate/3,
    gmdate/4,
    date_default_timezone_set/3,
    date_default_timezone_get/2
]).

-include("ephp.hrl").

-spec init() -> [ephp_func:php_function()].

init() -> [
    time, date, gmdate,
    date_default_timezone_get,
    date_default_timezone_set
]. 

-spec time(context(), line()) -> integer().

time(_Context, _Line) ->
    {MS,S,_} = os:timestamp(),
    MS * 1000000 + S.

-spec date(context(), line(), Format :: {variable(),binary()}) -> binary().

date(Context, _Line, {_,Format}) ->
    {MS,S,US} = os:timestamp(),
    date(Context, {"", Format}, {"", (MS * 1000000) + S + (US / 1000000)}).

-spec date(
    context(), line(),
    Format :: {variable(),binary()}, 
    Timestamp :: {variable(),(integer() | float())}) -> binary().

date(Context, _Line, {_,Format}, {_,Timestamp}) ->
    {M,S,U} = get_timestamp(Timestamp),
    TZ = ephp_context:get_tz(Context),
    Date = ezic:utc_to_local(calendar:now_to_universal_time({M,S,U}), TZ),
    date_format(Format, <<>>, {Timestamp, Date, TZ}).

-spec gmdate(context(), line(), Format :: {variable(),binary()}) -> binary().

gmdate(Context, _Line, {_,Format}) ->
    {MS,S,US} = os:timestamp(),
    gmdate(Context, Format, (MS * 1000000) + S + (US / 1000000)).

-spec gmdate(
    context(), line(),
    Format :: {variable(),binary()}, 
    Timestamp :: integer() | float()) -> binary().

gmdate(_Context, _Line, Format, Timestamp) ->
    {M,S,U} = get_timestamp(Timestamp),
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
    null.

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

-spec get_timestamp(TS::integer() | float()) -> timer:timestamp().

get_timestamp(Timestamp) ->
    M = trunc(Timestamp) div 1000000,
    S = trunc(Timestamp) rem 1000000,
    U = trunc(Timestamp * 1000000) rem 1000000,
    {M,S,U}.

-spec get_abbr_month(M :: integer()) -> binary().

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

-spec get_month(M :: integer()) -> binary().

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

-type date() :: {Year :: integer(), Month :: integer(), Day :: integer()}.

-spec get_abbr_weekday(D :: date()) -> binary().

get_abbr_weekday(0) -> <<"Sun">>;
get_abbr_weekday(1) -> <<"Mon">>;
get_abbr_weekday(2) -> <<"Tue">>;
get_abbr_weekday(3) -> <<"Wed">>;
get_abbr_weekday(4) -> <<"Thu">>;
get_abbr_weekday(5) -> <<"Fri">>;
get_abbr_weekday(6) -> <<"Sat">>;
get_abbr_weekday(7) -> <<"Sun">>.

-spec get_weekday(D :: date()) -> binary().

get_weekday(0) -> <<"Sunday">>;
get_weekday(1) -> <<"Monday">>;
get_weekday(2) -> <<"Tuesday">>;
get_weekday(3) -> <<"Wednesday">>;
get_weekday(4) -> <<"Thursday">>;
get_weekday(5) -> <<"Friday">>;
get_weekday(6) -> <<"Saturday">>;
get_weekday(7) -> <<"Sunday">>.

-spec date_format(ToAnalyze :: binary(), Result :: binary(), 
    {Timestamp :: integer(), Date :: date()}) -> binary().

date_format(<<>>, Result, _Date) ->
    Result;

date_format(<<"d",R/binary>>, Result, {_,{{_,_,D},_},_}=Date) -> 
    Day = ephp_util:pad_to_bin(D, 2), 
    date_format(R,<<Result/binary, Day/binary>>, Date);

date_format(<<"D",R/binary>>, Result, {_,{D,_},_}=Date) ->
    Day = get_abbr_weekday(D),
    date_format(R,<<Result/binary, Day/binary>>, Date);

date_format(<<"j",R/binary>>, Result, {_,{{_,M,_},_},_}=Date) ->
    Month = ephp_util:to_bin(M),
    date_format(R,<<Result/binary, Month/binary>>, Date);

date_format(<<"l",R/binary>>, Result, {_,{D,_},_}=Date) ->
    Day = get_weekday(D),
    date_format(R,<<Result/binary, Day/binary>>, Date);

date_format(<<"N",R/binary>>, Result, {_,{D,_},_}=Date) ->
    Day = calendar:day_of_the_week(D),
    DayBin = ephp_util:to_bin(if
        Day =:= 0 -> 7;
        true -> Day
    end),
    date_format(R,<<Result/binary, DayBin/binary>>, Date);

date_format(<<"S",R/binary>>, Result, {_,{{D,_,_},_},_}=Date) ->
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

date_format(<<"W",R/binary>>, Result, {_,{{Y,_,_},_},_}=Date) ->
    WeekOfYear = ephp_util:to_bin(calendar:iso_week_number(Y)),
    date_format(R,<<Result/binary, WeekOfYear/binary>>, Date);

date_format(<<"F",R/binary>>, Result, {_,{{_,M,_},_},_}=Date) ->
    Month = get_month(M),
    date_format(R,<<Result/binary, Month/binary>>, Date);

date_format(<<"m",R/binary>>, Result, {_,{{_,M,_},_},_}=Date) ->
    Month = ephp_util:pad_to_bin(M, 2),
    date_format(R,<<Result/binary, Month/binary>>, Date);

date_format(<<"M",R/binary>>, Result, {_,{{_,M,_},_},_}=Date) ->
    Month = get_abbr_month(M),
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

date_format(<<"B",R/binary>>, Result, {_,{_,{H,M,S}},_}=Date) ->
    Beats = trunc(((((H * 60) + M) * 60) + S) / 86.4),
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
    Daylight = case ezic:has_dst_local(D,TZ) of
        true -> <<"1">>;
        false -> <<"0">>
    end,
    date_format(R, <<Result/binary,Daylight/binary>>, Date);

date_format(<<"O",R/binary>>, Result, Date) ->
    %% TODO: timezone in +0000 or -0000 format.
    TimeZone = <<"O">>,
    date_format(R, <<Result/binary,TimeZone/binary>>, Date);

date_format(<<"P",R/binary>>, Result, Date) ->
    %% TODO: timezone in +00:00 or -00:00 format.
    TimeZone = <<"P">>,
    date_format(R, <<Result/binary,TimeZone/binary>>, Date);

date_format(<<"T",R/binary>>, Result, Date) ->
    %% TODO: timezone in EST, MDT, ... format.
    TimeZone = <<"T">>,
    date_format(R, <<Result/binary,TimeZone/binary>>, Date);

date_format(<<"Z",R/binary>>, Result, Date) ->
    %% TODO: timezone in +seconds format.
    TimeZone = <<"Z">>,
    date_format(R, <<Result/binary,TimeZone/binary>>, Date);

date_format(<<"c",R/binary>>, Result, {_,{{Y,M,D},{H,I,S}},_}=Date) ->
    YB = ephp_util:to_bin(Y),
    MB = ephp_util:pad_to_bin(M, 2),
    DB = ephp_util:pad_to_bin(D, 2),
    HB = ephp_util:pad_to_bin(H, 2),
    IB = ephp_util:pad_to_bin(I, 2),
    SB = ephp_util:pad_to_bin(S, 2),
    DateTime = <<YB/binary,"-",MB/binary,"-",DB/binary,"T",
        HB/binary,":",IB/binary,":",SB/binary>>,
    date_format(R, <<Result/binary,DateTime/binary>>, Date);

date_format(<<"r",R/binary>>, Result, {_,{{Y,M,D}=Dt,{H,I,S}},_}=Date) ->
    WB = get_abbr_weekday(Dt),
    DB = ephp_util:to_bin(D),
    MB = get_abbr_month(M),
    YB = ephp_util:to_bin(Y),
    HB = ephp_util:pad_to_bin(H, 2),
    IB = ephp_util:pad_to_bin(I, 2),
    SB = ephp_util:pad_to_bin(S, 2),
    %% TODO: add timezone in format +0000 or -0000.
    DateTime = <<WB/binary,", ",
        DB/binary," ",MB/binary," ",YB/binary," ",
        HB/binary,":",IB/binary,":",SB/binary>>,
    date_format(R, <<Result/binary,DateTime/binary>>, Date);

date_format(<<"U",R/binary>>, Result, {Timestamp,_,_}=Date) ->
    DateTime = ephp_util:to_bin(trunc(Timestamp)),
    date_format(R, <<Result/binary,DateTime/binary>>, Date);

date_format(<<Other:1/binary,R/binary>>, Result, Date) ->
    date_format(R, <<Result/binary,Other/binary>>, Date).
