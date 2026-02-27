<?php

require_once 'Util/Request.php';

$params = [
    'email' => 'midas_application@example.com',
    'password' => 'pass',
];

$noSessionRequest = new Request();
$response = $noSessionRequest->post('/login', $params);

$noSessionRequest = new Request($response->getSessionKey());
$response = $noSessionRequest->get('/sanctum/csrf-cookie', []);

$request = new Request($response->getSessionKey(), $response->getXsrfToken());

$response = $request->get('/balances');
var_dump($response->jsonBody());
var_dump($response->status());

$response = $request->post(
    '/balances',
    [
        'amount' => -500,
        'item' => "test",
        'kind_element_id' => 2,
        'purpose_element_id' => 3,
        'place_element_id' => 4,
        'date' => '2025-10-10',
    ],
);

var_dump($response->jsonBody());
var_dump($response->status());
$id = (int)$response->jsonBody();

$response = $request->put(
    '/balances/' . $id,
    [
        'amount' => -1000,
        'item' => "test",
        'kind_element_id' => 12,
        'purpose_element_id' => 13,
        'place_element_id' => 14,
        'date' => '2025-10-20',
    ],
);

var_dump($response->jsonBody());
var_dump($response->status());

$response = $request->delete('/balances/' . $id);
var_dump($response->jsonBody());
var_dump($response->status());

exit(1);
