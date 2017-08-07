<?php

function teardown() {
    print "goodbye!\n";
    trigger_error("whoops!", E_USER_ERROR);
}

register_shutdown_function("teardown");

