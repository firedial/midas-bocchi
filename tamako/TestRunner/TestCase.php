<?php

require_once __DIR__ . '/../Assert/Assert.php';
require_once __DIR__ . '/../Util/Request.php';

abstract class TestCase
{
    protected string $host;

    public function __construct(string $host)
    {
        $this->host = $host;
    }

    protected function getAuthenticatedRequest(): Request
    {
        $noSessionRequest = new Request();
        $response = $noSessionRequest->post('/login', [
            'email' => 'midas_application@example.com',
            'password' => 'pass',
        ]);

        $noSessionRequest = new Request($response->getSessionKey());
        $response = $noSessionRequest->get('/sanctum/csrf-cookie', []);

        return new Request($response->getSessionKey(), $response->getXsrfToken());
    }
}
