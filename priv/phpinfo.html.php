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
      <img src="<?= bragful_logo_uri() ?>" alt="Bragful"/>
    </a>
  </div>

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
        <img src="<?= bragful_logo_uri() ?>" alt="Bragful"/>
      </a>
    </div>
  </div>

  <div class="header">
    <h2>Core</h2>
  </div>

  <div class="container">
    <table>
      <tr>
        <th>PHP Version</th>
        <td><?=$_METADATA["version"]?></td>
      </tr>
    </table>
    <table>
      <thead>
        <th>Directive</th>
        <th>Value</th>
      </thead>
      <tbody>
        <? foreach ($_METADATA["directives"]["core"] as $key => $value) { ?>
        <tr>
          <td><?=$key?></td>
          <td><?=$value?></td>
        </tr>
        <? } ?>
      </tbody>
    </table>
  </div>

  <? foreach ($_METADATA["directives"] as $section => $directives) { ?>
  <? if ($section == "core") continue; ?>
  <div class="header">
    <h2><?=ucfirst($section)?></h2>
  </div>

  <div class="container">
    <table>
      <thead>
        <th>Directive</th>
        <th>Value</th>
      </thead>
      <tbody>
        <? foreach ($directives as $key => $value) { ?>
        <tr>
          <td><?=$key?></td>
          <td><?=$value?></td>
        </tr>
        <? } ?>
      </tbody>
    </table>
  </div>
  <? } ?>
</body>
</html>
