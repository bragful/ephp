<?php

class Data {
    private $internal_data = [];

    public function __get($idx) {
        print "retrieving $idx...\n";
        return $this->internal_data[$idx];
    }

    public function __set($idx, $value) {
        print "setting $idx to $value...\n";
        $this->internal_data[$idx] = $value;
        return $value;
    }

    public function __isset($idx) {
        print "checking $idx...\n";
        return isset($this->internal_data[$idx]);
    }

    public function __unset($idx) {
        print "unsetting $idx...\n";
        unset($this->internal_data[$idx]);
    }

    public function __toString() {
        return implode(", ", $this->internal_data);
    }
}

$data = new Data;
print "data->hi = " . $data->hi . "\n";
$data->hi = "hello";
$data->bye = "goodbye";
$data->another = ["other1", "other2", "other3"];
$data->another[] = "other4";
print "data->another = " . implode(", ", $data->another) . "\n";
print "data->another[0] = " . $data->another[0] . "\n";
print "data->hi = " . $data->hi . "\n";
unset($data->another);
print "content: $data\n";

