<?php

if (0 == 1) {
if ( $api_key = Akismet::get_api_key() && ( empty( self::$notices['status'] ) || 'existing-key-invalid' != self::$notices['status'] ) ) {
                        self::display_configuration_page();
                        return;
                }
}
// this test is only to check there are no parser errors
print "OK\n";
