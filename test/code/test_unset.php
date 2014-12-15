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

unset($users[1]);

?>
<html>
<head>
    <title>My Web Example</title>
</head>
<body>
    <pre><?=print_r($users,true)?></pre>
</body>
</html>

