<?php

$s = '';
if (isset($_GET['username'] )) { $username = $_GET['username'];  $s = $s .'GET ' . $username;}
if (isset($_POST['username'])) { $username = $_POST['username']; $s = $s .'POST '. $username;}

echo "$s";
if ($s != '') { exit(); }
?>

<html>
<body>
<form name="form1" method="GET" action="index.php"> <b>get</b><br>
username: <input type="text" name="username"><br> <br> <input type="submit" value="submit"> </form>

<form name="form2" method="POST" action="index.php"> <b>post</b><br>
username: <input type="text" name="username"><br> <br> <input type="submit" value="submit"> </form> </body> </html>