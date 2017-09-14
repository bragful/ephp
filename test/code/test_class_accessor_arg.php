<?php

$mode = 10;
if ($mode != 0) $block = preg_replace("/\r\n|\r|\n/", $this->_eol_code[$this->OS_local], $block);

