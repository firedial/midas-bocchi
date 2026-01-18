<?php

$c = curl_init();
// curl_setopt($c, CURLOPT_URL, "http://haruhi/api/attribute_elements/kind_element");
curl_setopt($c, CURLOPT_URL, "http://haruhi/api/login");
curl_setopt($c, CURLOPT_POST, true);
curl_setopt($c, CURLOPT_RETURNTRANSFER, true);
curl_setopt($c, CURLOPT_SSL_VERIFYPEER, false);
curl_setopt($c, CURLOPT_HEADER, true);
// curl_setopt($c, CURLOPT_COOKIEFILE, 'cookies.txt');

$headers = [
    'Content-Type: application/json',
    'Accept-Charset: UTF-8',
];
$params = [
    'email' => 'midas_application@example.com',
    'password' => 'pass',
];

curl_setopt($c, CURLOPT_POSTFIELDS, json_encode($params));
curl_setopt($c, CURLOPT_HTTPHEADER, $headers);
curl_setopt($c, CURLOPT_COOKIEJAR, '/tmp/cookies.txt');

$r =  curl_exec($c);
$data = json_decode($r, true);
var_dump(curl_getinfo($c));

// ヘッダーサイズを取得
$header_size = curl_getinfo($c, CURLINFO_HEADER_SIZE);

// ヘッダーとボディを分離
$header = substr($r, 0, $header_size);
$body = substr($r, $header_size);

var_dump($header);

unset($c);
