<?php
$input = "Alien";
echo "[";
echo str_pad($input, 10);                      // produce "Alien     "
echo "]\n[";
echo str_pad($input, 10, "-=", STR_PAD_LEFT);  // produce "-=-=-Alien"
echo "]\n[";
echo str_pad($input, 10, "_", STR_PAD_BOTH);   // produce "__Alien___"
echo "]\n[";
echo str_pad($input,  6, "___");               // produces "Alien_"
echo "]\n[";
echo str_pad($input,  3, "*");                 // produces "Alien"
echo "]\n[";
echo str_pad($input,  19, ".,_", STR_PAD_BOTH);  // produces "Alien"
echo "]\n";
