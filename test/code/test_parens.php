<?php

class wordpress {
    public function get_var($sql) {
        return true;
    }
}

$wpdb = new wordpress;
$sql = "ok";

$user_table = ( $wpdb->get_var( $sql ) != null );
var_dump($user_table);
