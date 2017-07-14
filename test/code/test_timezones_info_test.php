<?php

$timezones = timezone_abbreviations_list();

$a = [];
print "timezone_test() ->\n";
foreach ($timezones as $timezone => $timezone_infos) {
        foreach ($timezone_infos as $timezone_info) {
		if (empty($timezone_info['timezone_id'])) continue;
		$dst = $timezone_info['dst'] ? "true" : "false";
		$timezone_id = $timezone_info['timezone_id'];
		$upper_timezone = strtoupper($timezone);
		if (isset($a[$timezone_id]) and isset($a[$timezone_id][$dst])) continue;
		$a[$timezone_id][$dst] = true;
		print "    ?assertEqual(<<\"$upper_timezone\">>, ephp_timezone:timezone_to_abbreviation($dst, <<\"$timezone_id\">>)),\n";
	}
}
print "    ok.\n";

