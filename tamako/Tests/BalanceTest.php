<?php

require_once __DIR__ . '/../TestRunner/TestCase.php';

class BalanceTest extends TestCase
{
    public function testBalanceGet(): void
    {
        $assert = new Assert();
        $request = $this->getAuthenticatedRequest();

        $response = $request->get('/balances');
        $assert->isStatusCode200($response->statusCode());
    }
}
