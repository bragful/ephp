<?

$users = array ( 
	array (
		"name" => "Manuel",
		"surname" => "Rubio",
	), array (
		"name" => "Margarita",
		"surname" => "Ortiz",
	)
);

?>
<html>
<head>
    <title>My Web Example</title>
</head>
<body>
    <table>
    <tr>
        <th>Name</th>
        <th>Surname</th>
    </tr>
    <? foreach ($users as $user) { ?>
    <tr>
        <td><?=$user['name']?></td>
        <td><?=$user['surname']?></td>
    </tr>
    <? if ($user['surname'] == 'Rubio') return; ?>
    <? } ?>
</body>
</html>

