PHP Version => <?=$_METADATA["version"]?> 

System => Erlang/OTP <?=$_METADATA["release"]?>  ephp <?=$_METADATA["vsn"]?> 
Build Date => <?=$_METADATA["build_date"]?> 
Configuration File (php.ini) Path => <?=$_METADATA["path_php_ini"]?> 

Core

PHP Version => <?=$_METADATA["version"]?> 

Directive => Value
<? foreach ($_METADATA["directives"] as $key => $value) { ?>
<?=$key?> => <?=$value?> 
<? } ?>
