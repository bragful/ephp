<?php

date_default_timezone_set("UTC");

ini_set("error_log", "");

// sent to SAPI
error_log("this is an error", 0);
error_log("this is another one", 4);
error_log("this is an error via email", 1, "mail@mail.com");
error_log("this is an error via email", 1, "mail@mail.com", "X-Sent: PHP");

ini_set("error_log", "file.log");

// sent to file
error_log("this is an error", 0);
error_log("this is an error for destination", 3, "file.log");

ini_set("error_log", "syslog");

// sent to syslog
error_log("this is a system error", 0);
