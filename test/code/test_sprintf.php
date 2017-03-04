<?php

echo sprintf("%%h (no existent) = %h\n");
echo sprintf("%%'.-20s = %'.-20s\n", "hello");
echo sprintf("%%'.20s  = %'.20s\n", "hello");
echo sprintf("%%2s     = %2s\n", "hello");

