-----------
 MICROTIME
-----------

<?php

date_default_timezone_set('UTC');

print "microtime() -> " . microtime() . "\n";
print "microtime(true) -> " . microtime(true) . "\n";
print "microtime(false) -> " . microtime(false) . "\n";

print date_default_timezone_get() . "\n";

?>

------
 DATE
------

<?php

print "date('Y-m-d') -> " . date("Y-m-d") . "\n";
print "gmdate('Y-m-d') -> " . gmdate('Y-m-d') . "\n";

$dates = [
    // 2015-03-02 10:30:10 (Tue)
    1422959410,

    // 2016-01-01 09:30:10 (Fri)
    1451640610,

    // 2016-02-02 10:30:10 (Tue)
    1454409010,

    // 2016-02-03 10:30:10 (Wed)
    1454495410,

    // 2016-03-03 11:30:10 (Thu)
    1457004610,

    // 2016-04-04 12:30:10 (Mon)
    1459773010,

    // 2016-05-05 13:30:10 (Thu)
    1462455010,

    // 2016-06-06 14:30:10 (Mon)
    1465223410,

    // 2016-07-07 15:30:10 (Thu)
    1467905410,

    // 2016-08-08 16:30:10 (Mon)
    1470673810,

    // 2016-09-09 17:30:10 (Fri)
    1473442210,

    // 2016-10-10 18:30:10 (Mon)
    1476124210,

    // 2016-11-11 19:30:10 (Fri)
    1478892610,

    // 2016-12-10 20:30:10 (Sat)
    1481401810,

    // 2016-12-11 20:30:10 (Sun)
    1481488210,

    // 2016-12-12 20:30:10 (Mon)
    1481574610,
];

$formats = [
    // 2016-09-09 17:30:10.000000
    "easy" => "Y-m-d H:i:s.u",

    // Fri, 09 Sep 2016 17:30:10 +0000
    "english" => "r",

    // 2016-09-09T17:30:10
    "iso" => "c",

    // 1473442210
    "posix" => "U",

    // 1st September 16 (Sep=9[30]) 5:30:10pm (Friday;5)
    "info" => "jS F y (M=n[t]) g:i:sa (l;N)",

    // 0
    "leapyear?" => "L",

    // Fri 5
    "day of week" => "D w",

    // 252
    "day of year" => "z",

    // 36 - 2016
    "week of year / year of week" => "W - o",

    // 17 17 5 05 770
    "hours" => "h H g G [A] B",

    // UTC (0)
    "timezone" => "e (I)",

    // +3600 UTC +01:00 +0100
    "Z T P O",
];

$timezones = [
    "UTC",
    "GMT",
    "Europe/Madrid",
    "America/Mexico_City",
    "Inexistent",
];

foreach ($dates as $date) {
    foreach ($formats as $name => $format) {
        foreach ($timezones as $timezone) {
            date_default_timezone_set($timezone);
            print "\n// $name in $timezone\n";
            print "date('$format', $date) -> " . date($format, $date) . "\n";
        }
    }
}
