<?php

user_error("This message is not important", E_USER_NOTICE);
user_error("This message could be important", E_USER_WARNING);
user_error("This message breaks the system!", E_USER_ERROR);

print "NO OK\n";
