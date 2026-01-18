<?php

$c = curl_init();
curl_setopt($c, CURLOPT_URL, "http://haruhi/api/attribute_elements/kind_element");
curl_setopt($c, CURLOPT_RETURNTRANSFER, true);
curl_setopt($c, CURLOPT_SSL_VERIFYPEER, false);
$r =  curl_exec($c);
$data = json_decode($r, true);
var_dump($data);
