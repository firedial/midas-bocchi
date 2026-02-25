<?php

require_once 'Util/Request.php';

$params = [
    'email' => 'midas_application@example.com',
    'password' => 'pass',
];

$noSessionRequest = new Request();
$response = $noSessionRequest->post('/login', $params);

$response = $noSessionRequest->get('/attribute_elements/kind_element');
var_dump($response->jsonBody());
echo $response->status();

$request = new Request($response->getSessionKey());
// $response = $request->get('/balances');
$response = $request->get('/attribute_elements/kind_element');
var_dump($response->jsonBody());
echo $response->status();
