phpinfo()
<? if ($_FLAGS & INFO_GENERAL) { ?>
PHP Version => <?=$_METADATA["version"]?> 

System => Erlang/OTP <?=$_METADATA["release"]?>  ephp <?=$_METADATA["vsn"]?> 
Build Date => <?=$_METADATA["build_date"]?> 
Configuration File (php.ini) Path => <?=$_METADATA["path_php_ini"]?> 
Registered PHP Streams => <?=join(",", $_METADATA["streams"])?> 

This program makes use of the Bragful ePHP Engine: ephp; Erlang is a
trademark of Ericsson and PHP is a trademark of Zend Technologies.
<? } ?>
<? if ($_FLAGS & INFO_CONFIGURATION) { ?>
 _______________________________________________________________________

<? foreach ($_METADATA["directives"] as $section => $directives) { ?>
<? switch ($section) { ?>
<? case "core": ?>
Core

PHP Version => <?=$_METADATA["version"]?> 

<?   break; ?>
<? case "date": ?>
date

date/time support => enabled
timelib version => <?=$_METADATA["tz_version"]?>
"Olson" Timezone Database Version => <?=$_METADATA["tz_version"]?>
Timezone Database => ezic
Default timezone => UTC

<?   break; ?>
<? case "pcre": ?>
pcre

PCRE (Perl Compatible Regular Expressions) Support => enabled
PCRE Library Version => <?=PCRE_VERSION?>
PCRE JIT Support => not compiled in
<?   break; ?>
<? default: ?>
<?=ucfirst($section)?> 

<? } ?>
Directive => Value
<? foreach ($directives as $key => $value) { ?>
<?=$key?> => <?=$value ?? "no value"?> 
<? } ?>

<? } ?>
<? } ?>
<? if ($_FLAGS & INFO_ENVIRONMENT) { ?>
Environment

Variable => Value
<? foreach ($_ENV as $key => $value) { ?>
<?=$key?> => <?=$value?> 
<? } ?>

<? } ?>
<? if ($_FLAGS & INFO_VARIABLES) { ?>
PHP Variables

Variable => Value
<? foreach ($_SERVER as $key => $value) { ?>
$_SERVER['<?=$key?>'] => <?=print_r($value, true)?> 
<? } ?>
<? foreach ($_ENV as $key => $value) { ?>
$_ENV['<?=$key?>'] => <?=print_r($value, true)?> 
<? } ?>

<? } ?>
<? if ($_FLAGS & INFO_LICENSE) { ?>
License

<?=$LICENSE?>
<? } ?>
