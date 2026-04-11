<?php

require_once __DIR__ . '/../TestRunner/TestCase.php';

class LoginTest extends TestCase
{
    public function testAuthOk(): void
    {
        $response = $this->request->get('/balances');
        Assert::assertStatusCode200($response->statusCode());
    }

    public function testAuthNgWithoutKey(): void
    {
        $noAuthRequest = new Request();
        $response = $noAuthRequest->get('/balances');
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証情報なし');
    }

    public function testAuthNgWithWrongKey(): void
    {
        $wrongKeyRequest = new Request('wrong-key');
        $response = $wrongKeyRequest->get('/balances');
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], 'キー不一致');
    }
}
