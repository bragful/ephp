<!DOCUMENT html>
<html>
<head>
  <title>Bragful phpinfo()</title>
</head>
<body>
  <div class="header">
    <h1>PHP Version <?=$_METADATA["version"]?></h1>
    <a href="https://bragful.com"><img src="" alt="Bragful"/></a>
  </div>

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
  </table>

  <div class="header">
    <h2>Core</h2>
  </div>

  <table>
    <tr>
      <th>PHP Version</th>
      <td><?=$_METADATA["version"]?></td>
    </tr>
    <tr>
      <th>Directive</th>
      <th>Value</th>
    </tr>
    <? foreach ($_METADATA["directives"] as $key => $value) { ?>
    <tr>
      <td><?=$key?></td>
      <td><?=$value?></td>
    </tr>
    <? } ?>
  </table>
</body>
</html>
