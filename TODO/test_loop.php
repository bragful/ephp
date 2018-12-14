<?php

class Data {
    public function __get($idx) {
        print "retrieving...\n";
        return $this->internal_data[$idx];
    }
}

$data = new Data;
print $data->hi . "\n";

