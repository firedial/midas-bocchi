<?php

require_once __DIR__ . '/../TestRunner/TestCase.php';

class BalanceTest extends TestCase
{
    private function getAuthenticatedRequest(): Request
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

    public function testBalanceGet(): void
    {
        $assert = new Assert();
        $request = $this->getAuthenticatedRequest();

        $response = $request->get('/balances');
        $assert->is200($response->status());
    }
}
