<?php

require_once 'Util/Request.php';

$params = [
    'email' => 'midas_application@example.com',
    'password' => 'pass',
];

$noSessionRequest = new Request();
$response = $noSessionRequest->post('/login', $params);

$request = new Request($response->getSessionKey());
$response = $request->get('/balances');

var_dump($response->status());
