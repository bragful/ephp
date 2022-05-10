<?php

function get_post_type( $value ) {
    return 'excerpt' === $value || 'full' === $value ? $value : 'excerpt';
};

var_dump(get_post_type('excerpt'));
var_dump(get_post_type('full'));
var_dump(get_post_type('other'));
