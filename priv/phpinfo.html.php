<!DOCUMENT html>
<html>
<head>
  <title>Bragful phpinfo()</title>
  <style type="text/css">
  body, h1, h2, p, a, th, td {
    font-family: "Consolas", monospace;
    font-size: 9pt;
    text-align: left;
  }
  .header {
    border: 1px solid #3d82bf;
    margin: 5px;
    padding: 5px 10px;
    background-color: #3d82bf;
    color: white;
  }
  #bragful-logo img {
    width: 50px;
    float: right;
    position: absolute;
    right: 20px;
    margin-top: -50px;
  }
  h1 {
    font-size: 16pt;
  }
  h2 {
    font-size: 14pt;
  }
  h3 {
    font-size: 12pt;
  }
  .container {
    margin: 5px auto 5px auto;
    padding: 5px;
  }
  .info {
    border: 1px solid lightgray;
    padding: 5px 20px;
    margin-top: 20px;
    background-color: lightgray;
    margin: 20px 0px 0px 0px;
  }
  .info p {
    margin-right: 52px;
  }
  .info a {
    text-decoration: underline;
    font-weight: bolder;
    color: black;
  }
  th, td {
    border: 1px solid;
    margin: 1px;
    padding: 5px;
  }
  th {
    background-color: #3d82bf;
    color: white;
    border-color: #3d82bf;
  }
  td {
    background-color: lightgray;
    border-color: lightgray;
  }
  table {
    margin-left: auto;
    margin-right: auto;
    width: 95%;
  }
  table th, table td {
    width: 50%;
  }
  </style>
</head>
<body>
  <div class="header">
    <h1>PHP Version <?=$_METADATA["version"]?></h1>
    <a id="bragful-logo" href="https://bragful.com">
      <img src="<?= php_logo_guid() ?>" alt="Bragful"/>
    </a>
  </div>

  <? if ($_FLAGS & INFO_GENERAL) { ?>
  <div class="container">
    <table>
      <tr>
        <th>System</th>
        <td>Erlang/OTP <?=$_METADATA["release"]?> ephp <?=$_METADATA["vsn"]?></td>
      </tr>
      <tr>
        <th>Build Date</th>
        <td><?=$_METADATA["build_date"]?></td>
      </tr>
      <tr>
        <th>Configuration File (php.ini) Path</th>
        <td><?=$_METADATA["path_php_ini"]?></td>
      </tr>
      <tr>
        <th>Registered PHP Streams</th>
        <td><?=join(",", $_METADATA["streams"])?></td>
    </table>

    <div class="info">
      <p>This program makes use of the <strong>Bragful ePHP</strong> Engine: <a href="https://github.com/bragful/ephp">ephp</a>; Erlang is a trademark of Ericsson and PHP is a trademark of Zend Technologies.</p>
      <a id="bragful-logo" href="https://bragful.com">
        <img src="<?= php_logo_guid() ?>" alt="Bragful"/>
      </a>
    </div>
  </div>
  <? } ?>

  <? if ($_FLAGS & INFO_CONFIGURATION) { ?>
  <div class="header">
    <h2>Configuration</h2>
  </div>

  <? foreach ($_METADATA["directives"] as $section => $directives) { ?>
  <? switch ($section) { ?>
<? case "core": ?>
  <div class="header">
    <h3>Core</h3>
  </div>

  <div class="container">
    <table>
      <tr>
        <th>PHP Version</th>
        <td><?=$_METADATA["version"]?></td>
      </tr>
    </table>

  <?   break; ?>
  <? case "date": ?>
  <div class="header">
    <h3>date</h3>
  </div>

  <div class="container">
    <table>
      <tr>
        <th>date/time support</th>
        <td>enabled</td>
      </tr>
      <tr>
        <th>timelib version</th>
        <td><?=$_METADATA["tz_version"]?></th>
      </tr>
      <tr>
        <th>"Olson" Timezone Database Version</th>
        <td><?=$_METADATA["tz_version"]?></td>
      </tr>
      <tr>
        <th>Timezone Database</th>
        <td>ezic</td>
      </tr>
      <tr>
        <th>Default timezone</th>
        <td>UTC</td>
      </tr>
    </table>

  <?   break; ?>
  <? case "pcre": ?>
  <div class="header">
    <h3>pcre</h3>
  </div>

  <div class="container">
    <table>
      <tr>
        <th>PCRE (Perl Compatible Regular Expressions) Support</th>
        <td>enabled</td>
      </tr>
      <tr>
        <th>PCRE Library Version</th>
        <td><?=PCRE_VERSION?></td>
      </tr>
      <tr>
        <th>PCRE JIT Support</th>
        <td>not compiled in</td>
      </tr>
    </table>

  <?   break; ?>
  <? default: ?>
  <div class="header">
    <h3><?=ucfirst($section)?></h3>
  </div>

  <div class="container">

  <? } ?>
    <table>
      <thead>
        <th>Directive</th>
        <th>Value</th>
      </thead>
      <tbody>
      <? foreach ($directives as $key => $value) { ?>
        <tr>
          <td><?=$key?></td>
          <td><?=$value ?? "no value"?></td>
        </tr>
      <? } ?>
      </tbody>
    </table>
  </div>
  <? } ?>
<? } ?>

<? if ($_FLAGS & INFO_ENVIRONMENT) { ?>
  <div class="header">
    <h2>Environment</h2>
  </div>

  <div class="container">
    <table>
      <thead>
        <th>Variable</th>
        <th>Value</th>
      </thead>
      <tbody>
      <? foreach ($_ENV as $key => $value) { ?>
        <tr>
          <td><?=$key?></td>
          <td><?=$value?></td>
        </tr>
      <? } ?>
      </tbody>
    </table>
  </div>
<? } ?>
<? if ($_FLAGS & INFO_VARIABLES) { ?>
  <div class="header">
    <h2>PHP Variables</h2>
  </div>

  <div class="container">
    <table>
      <thead>
        <th>Variable</th>
        <th>Value</th>
      </thead>
      <tbody>
      <? foreach ($_SERVER as $key => $value) { ?>
        <tr>
          <td>$_SERVER['<?=$key?>']</td>
          <td><?=print_r($value, true)?></td>
        </tr>
      <? } ?>
      <? foreach ($_ENV as $key => $value) { ?>
        <tr>
          <td>$_ENV['<?=$key?>']</td>
          <td><?=print_r($value, true)?></td>
        </tr>
      <? } ?>
      </tbody>
    </table>
  </div>

<? } ?>
<? if ($_FLAGS & INFO_LICENSE) { ?>
  <div class="header">
    <h2>License</h2>
  </div>

  <div class="container">
    <div class="info">
      <p><?=$LICENSE?></p>
      <a id="bragful-logo" href="https://bragful.com">
        <img src="<?= php_logo_guid() ?>" alt="Bragful"/>
      </a>
    </div>
  </div>

<? } ?>
</body>
</html>
