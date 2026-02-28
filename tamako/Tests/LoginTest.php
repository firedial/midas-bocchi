<?php

require_once __DIR__ . '/../TestRunner/TestCase.php';

class LoginTest extends TestCase
{
    public function testLoginOk(): void
    {
        $noSessionRequest = new Request();
        $response = $noSessionRequest->post('/login', [
            'email' => 'midas_application@example.com',
            'password' => 'pass',
        ]);

        Assert::assertStatusCode200($response->statusCode());
    }

    public function testLoginNg(): void
    {
        $assert = new Assert();

        // パスワードが違うパターン
        $noSessionRequest = new Request();
        $response = $noSessionRequest->post('/login', [
            'email' => 'midas_application@example.com',
            'password' => 'pass1111',
        ]);

        Assert::assertStatusCode401($response->statusCode());
    }

    public function testLoginInvalidParam(): void
    {
        $assert = new Assert();

        // パスワードがない
        $noSessionRequest = new Request();
        $response = $noSessionRequest->post('/login', [
            'email' => 'midas_application@example.com',
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // Eメールがない
        $noSessionRequest = new Request();
        $response = $noSessionRequest->post('/login', [
            'password' => 'pass',
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // Eメールの形式ではない
        $noSessionRequest = new Request();
        $response = $noSessionRequest->post('/login', [
            'email' => 'midas_application_example.com',
            'password' => 'pass',
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    public function testLogoutOk(): void
    {
        $assert = new Assert();

        // 認証通っているかの確認
        $request = $this->getAuthenticatedRequest();
        $response = $request->get('/balances');
        Assert::assertStatusCode200($response->statusCode());

        // ログアウト
        $response = $request->post('/logout');
        Assert::assertStatusCode200($response->statusCode());

        // 認証通らないことの確認
        $response = $request->get('/balances');
        Assert::assertStatusCode401($response->statusCode());

        // ログアウト状態でのログアウト処理
        $response = $request->post('/logout');
        Assert::assertStatusCode200($response->statusCode());
    }
}
