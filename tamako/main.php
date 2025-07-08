<?php

$pdo = new PDO("mysql:host=yui;dbname=midas", "midas", "midaspass");
$stmt = $pdo->query("SELECT * FROM m_balance");

var_dump($stmt->fetchAll());
