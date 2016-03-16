<?php

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
    <pre><?=print_r($users,true)?></pre>
</body>
</html>

