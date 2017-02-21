<?php

function call_me() {
    call_me_maybe();
}

@call_me();
print "hello world!";
@call_me_maybe();
print "again!";

