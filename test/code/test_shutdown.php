<?php

function teardown() {
    print "goodbye!\n";
}

register_shutdown_function("teardown");

