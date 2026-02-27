<?php

require_once __DIR__ . '/../TestRunner/TestCase.php';

class LoginTest extends TestCase
{
    public function testLoginOk(): void
    {
        $assert = new Assert();

        $noSessionRequest = new Request();
        $response = $noSessionRequest->post('/login', [
            'email' => 'midas_application@example.com',
            'password' => 'pass',
        ]);

        $assert->is200($response->status());
    }

    public function testLoginNg(): void
    {
        $assert = new Assert();

        $noSessionRequest = new Request();
        $response = $noSessionRequest->post('/login', [
            'email' => 'midas_application@example.com',
            'password' => 'pass1111',
        ]);

        $assert->is401($response->status());
    }
}
