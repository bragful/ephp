-module(ephp_func_date).
-compile([warnings_as_errors]).

-export([
    init/1,
    time/1,
    date/2,
    date/3,
    gmdate/2,
    gmdate/3
]).

-include("ephp.hrl").

-spec init(Context :: context()) -> ok.

init(Context) ->
    ephp_context:register_func(Context, <<"time">>, ?MODULE, time),
    ephp_context:register_func(Context, <<"date">>, ?MODULE, date),
    ephp_context:register_func(Context, <<"gmdate">>, ?MODULE, gmdate),
    ok. 

-spec time(Context :: context()) -> integer().

time(_Context) ->
    {MS,S,_} = os:timestamp(),
    MS * 1000000 + S.

-spec date(Context :: context(), Format :: binary()) -> binary().

date(Context, Format) ->
    {MS,S,US} = os:timestamp(),
    date(Context, Format, (MS * 1000000) + S + (US / 1000000)).

-spec date(Context :: context(), Format :: binary(), Timestamp :: integer() | float()) -> binary().

date(Context, Format, Timestamp) ->
    M = trunc(Timestamp / 1000000),
    S = trunc(Timestamp - (M * 1000000)),
    U = (Timestamp - S) * 1000000,
    TZ = ephp_context:get_tz(Context),
    Date = ezic:utc_to_local(calendar:now_to_universal_time({M,S,U}), TZ),
    date_format(Format, <<>>, {Timestamp, Date, TZ}).

-spec gmdate(Context :: context(), Format :: binary()) -> binary().

gmdate(Context, Format) ->
    {MS,S,US} = os:timestamp(),
    gmdate(Context, Format, (MS * 1000000) + S + (US / 1000000)).

-spec gmdate(Context :: context(), Format :: binary(), Timestamp :: integer() | float()) -> binary().

gmdate(_Context, Format, Timestamp) ->
    M = trunc(Timestamp / 1000000),
    S = trunc(Timestamp - (M * 1000000)),
    U = (Timestamp - S) * 1000000,
    TZ = "GMT",
    Date = calendar:now_to_universal_time({M,S,U}),
    date_format(Format, <<>>, {Timestamp, Date, TZ}).

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

-spec get_abbr_month(M :: integer()) -> binary().

get_abbr_month(M) ->
    case M of
        1 -> <<"Jan">>;
        2 -> <<"Feb">>;
        3 -> <<"Mar">>;
        4 -> <<"Apr">>;
        5 -> <<"May">>;
        6 -> <<"Jun">>;
        7 -> <<"Jul">>;
        8 -> <<"Aug">>;
        9 -> <<"Sep">>;
        10 -> <<"Oct">>;
        11 -> <<"Nov">>;
        12 -> <<"Dec">>
    end.

-spec get_month(M :: integer()) -> binary().

get_month(M) ->
    case M of
        1 -> <<"January">>;
        2 -> <<"February">>;
        3 -> <<"March">>;
        4 -> <<"April">>;
        5 -> <<"May">>;
        6 -> <<"June">>;
        7 -> <<"July">>;
        8 -> <<"August">>;
        9 -> <<"September">>;
        10 -> <<"October">>;
        11 -> <<"November">>;
        12 -> <<"December">>
    end.

-type date() :: {Year :: integer(), Month :: integer(), Day :: integer()}.

-spec get_abbr_weekday(D :: date()) -> binary().

get_abbr_weekday(D) ->
    case calendar:day_of_the_week(D) of
        0 -> <<"Sun">>;
        1 -> <<"Mon">>;
        2 -> <<"Tue">>;
        3 -> <<"Wed">>;
        4 -> <<"Thu">>;
        5 -> <<"Fri">>;
        6 -> <<"Sat">>;
        7 -> <<"Sun">>
    end.

-spec get_weekday(D :: date()) -> binary().

get_weekday(D) ->
    case calendar:day_of_the_week(D) of
        0 -> <<"Sunday">>;
        1 -> <<"Monday">>;
        2 -> <<"Tuesday">>;
        3 -> <<"Wednesday">>;
        4 -> <<"Thursday">>;
        5 -> <<"Friday">>;
        6 -> <<"Saturday">>;
        7 -> <<"Sunday">>
    end.

-spec date_format(ToAnalyze :: binary(), Result :: binary(), 
    {Timestamp :: integer(), Date :: date()}) -> binary().

date_format(<<>>, Result, _Date) ->
    Result;

date_format(<<"d",R/binary>>, Result, {_,{{_,_,D},_},_}=Date) -> 
    DB = ephp_util:to_bin(D), 
    Day = if 
        byte_size(D) =:= 1 -> <<"0",DB/binary>>;
        true -> DB
    end,
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
    FullYear = ephp_util:to_bin(Y),
    Size = byte_size(FullYear) - 2,
    Year = if
        Size >= 1 -> 
            <<_:Size/binary, YearPart:2/binary>> = FullYear,
            YearPart;
        Size =:= 0 -> FullYear;
        Size =:= -1 -> <<"0",FullYear/binary>>
    end,
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
    Hour = ephp_util:to_bin(if
        H >= 1 andalso H =< 12 -> H;
        H >= 12 andalso H =< 24 -> H-12;
        H =:= 0 -> 12
    end),
    date_format(R, <<Result/binary,Hour/binary>>, Date);

date_format(<<"G",R/binary>>, Result, {_,{_,{H,_,_}},_}=Date) ->
    Hour = ephp_util:to_bin(H),
    date_format(R, <<Result/binary,Hour/binary>>, Date);

date_format(<<"h",R/binary>>, Result, {_,{_,{H,_,_}},_}=Date) ->
    Hour = ephp_util:pad_to_bin(if
        H >= 1 andalso H =< 12 -> H;
        H >= 12 andalso H =< 24 -> H-12;
        H =:= 0 -> 12
    end, 2),
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
    US = trunc((Timestamp - trunc(Timestamp)) * 1000000),
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
    %% TODO: get timezone.
    TimeZone = <<"O">>,
    date_format(R, <<Result/binary,TimeZone/binary>>, Date);

date_format(<<"P",R/binary>>, Result, Date) ->
    %% TODO: get timezone.
    TimeZone = <<"P">>,
    date_format(R, <<Result/binary,TimeZone/binary>>, Date);

date_format(<<"T",R/binary>>, Result, Date) ->
    %% TODO: get timezone.
    TimeZone = <<"T">>,
    date_format(R, <<Result/binary,TimeZone/binary>>, Date);

date_format(<<"Z",R/binary>>, Result, Date) ->
    %% TODO: get timezone.
    TimeZone = <<"Z">>,
    date_format(R, <<Result/binary,TimeZone/binary>>, Date);

date_format(<<"c",R/binary>>, Result, {_,{{Y,M,D},{H,I,S}},_}=Date) ->
    DateTime = iolist_to_binary(io_lib:format(
        "~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b",
        [Y,M,D,H,I,S])), 
    date_format(R, <<Result/binary,DateTime/binary>>, Date);

date_format(<<"r",R/binary>>, Result, {_,{{Y,M,D}=Dt,{H,I,S}},_}=Date) ->
    DateTime = iolist_to_binary(io_lib:format(
        "~w, ~b ~w ~b ~2..0b:~2..0b:~2..0b",
        [get_abbr_weekday(Dt),D,M,Y,H,I,S])),
    date_format(R, <<Result/binary,DateTime/binary>>, Date);

date_format(<<"U",R/binary>>, Result, {Timestamp,_,_}=Date) ->
    DateTime = ephp_util:to_bin(trunc(Timestamp)),
    date_format(R, <<Result/binary,DateTime/binary>>, Date);

date_format(<<Other:1/binary,R/binary>>, Result, Date) ->
    date_format(R, <<Result/binary,Other/binary>>, Date).
