<?php

$timezones = timezone_abbreviations_list();

foreach ($timezones as $timezone => $timezone_infos) {
	print "timezone_info(<<\"" . strtoupper($timezone) . "\">>) -> [\n";
	$a = [];
        foreach ($timezone_infos as $timezone_info) {
		$dst = $timezone_info['dst'] ? "true" : "false";
		$offset = $timezone_info['offset'];
		$timezone_id = $timezone_info['timezone_id'];
		$a[] = "    {" . $dst . ", $offset, <<\"$timezone_id\">>}";
	}
	print join(",\n", $a) . "\n];\n";
}
print "timezone_info(_) -> undefined.\n";

