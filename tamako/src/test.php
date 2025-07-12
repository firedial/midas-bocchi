<?php

$pdo = new PDO("mysql:host=azusa;dbname=midas", "midas", "midaspass");
$stmt = $pdo->query("SELECT * FROM m_kind_element");

var_dump($stmt->fetchAll());

$url = "http://haruhi/api/attribute_categories/kind_category";
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$r = curl_exec($ch);

var_dump($r);

curl_close($ch);
