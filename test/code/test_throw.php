<?php

function break_down($str) {
    echo "Launching...\n";
    throw new Exception;
}

break_down(["hello world!"]);
echo "here none lives!\n";
