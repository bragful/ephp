<?php

register_shutdown_function('handleFatalPhpError');

no_function_defined();

function handleFatalPhpError() {
   $last_error = error_get_last();
   if($last_error['type'] === E_ERROR) {
      echo "Can do custom output and/or logging for fatal error here...\n";
   }
   var_dump($last_error);
   echo "cleaning...\n";
   error_clear_last();
   var_dump(error_get_last());
   echo "Finish\n";
}
