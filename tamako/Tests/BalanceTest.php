<?php

require_once __DIR__ . '/../TestRunner/TestCase.php';

class BalanceTest extends TestCase
{
    /**
     * 収支一覧取得テスト
     */
    public function testBalanceGet(): void
    {
        $response = $this->request->get('/balances');
        Assert::assertStatusCode200($response->statusCode());
        $balances = $response->jsonBody();
        $beforeBalanceCount = count($balances);

        $response = $this->request->post('/balances', $this->validBalance());
        Assert::assertStatusCode200($response->statusCode());

        // レコードが 1 つ増えていることの確認
        $response = $this->request->get('/balances');
        Assert::assertStatusCode200($response->statusCode());
        $balances = $response->jsonBody();
        Assert::assertSame($beforeBalanceCount + 1, count($balances), '収支登録後のカウント');
    }

    /**
     * 収支CRUDテスト
     */
    public function testBalanceCRUD(): void
    {
        // 登録
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'テスト収支',
            'date' => '2021-05-01',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode200($response->statusCode());
        $id = $response->jsonBody();

        // 個別取得
        $response = $this->request->get('/balances/' . $id);
        Assert::assertStatusCode200($response->statusCode());

        $balance = $response->jsonBody();
        Assert::assertSame(-500, $balance['amount'], '収支登録後の amount');
        Assert::assertSame('テスト収支', $balance['item'], '収支登録後の item');
        Assert::assertSame('2021-05-01', $balance['date'], '収支登録後の date');
        Assert::assertSame(2, $balance['kind_element_id'], '収支登録後の kind_element_id');
        Assert::assertSame(3, $balance['purpose_element_id'], '収支登録後の purpose_element_id');
        Assert::assertSame(4, $balance['place_element_id'], '収支登録後の place_element_id');

        // 更新
        $response = $this->request->put('/balances/' . $id, [
            'amount' => -1000,
            'item' => 'テスト収支更新後',
            'date' => '2021-04-01',
            'kind_element_id' => 12,
            'purpose_element_id' => 13,
            'place_element_id' => 14,
        ]);
        Assert::assertStatusCode200($response->statusCode());

        // 更新後の値を確認
        $response = $this->request->get('/balances/' . $id);
        Assert::assertStatusCode200($response->statusCode());

        $balance = $response->jsonBody();
        Assert::assertSame(-1000, $balance['amount'], '更新後の amount');
        Assert::assertSame('テスト収支更新後', $balance['item'], '更新後の item');
        Assert::assertSame('2021-04-01', $balance['date'], '更新後の date');
        Assert::assertSame(12, $balance['kind_element_id'], '更新後の kind_element_id');
        Assert::assertSame(13, $balance['purpose_element_id'], '更新後の purpose_element_id');
        Assert::assertSame(14, $balance['place_element_id'], '更新後の place_element_id');

        // 削除
        $response = $this->request->delete('/balances/' . $id);
        Assert::assertStatusCode200($response->statusCode());

        // 削除後に取得すると 404 になること
        $response = $this->request->get('/balances/' . $id);
        Assert::assertStatusCode404($response->statusCode());
    }

    /**
     * 収支CRUDテスト
     */
    public function testBalanceLeapDay(): void
    {
        // 正常系

        // うるう年
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'テスト収支',
            'date' => '2024-02-29',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode200($response->statusCode());
        $id = $response->jsonBody();

        // 個別取得
        $response = $this->request->get('/balances/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $balance = $response->jsonBody();
        Assert::assertSame('2024-02-29', $balance['date'], '閏日の登録');

        // 更新
        $response = $this->request->put('/balances/' . $id, [
            'amount' => -1000,
            'item' => 'テスト収支更新後',
            'date' => '2020-02-29',
            'kind_element_id' => 12,
            'purpose_element_id' => 13,
            'place_element_id' => 14,
        ]);
        Assert::assertStatusCode200($response->statusCode());

        // 更新後の値を確認
        $response = $this->request->get('/balances/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $balance = $response->jsonBody();
        Assert::assertSame('2020-02-29', $balance['date'], '閏日の更新');

        // 異常系

        // うるう年
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'テスト収支',
            'date' => '2023-02-29',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // 更新
        $response = $this->request->put('/balances/' . $id, [
            'amount' => -1000,
            'item' => 'テスト収支更新後',
            'date' => '2022-02-29',
            'kind_element_id' => 12,
            'purpose_element_id' => 13,
            'place_element_id' => 14,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 存在しない収支テスト
     */
    public function testBalanceShowNotFound(): void
    {
        $response = $this->request->get('/balances/999999');
        Assert::assertStatusCode404($response->statusCode());
        $response = $this->request->put('/balances/999999', $this->validBalance());
        Assert::assertStatusCode404($response->statusCode());
        $response = $this->request->delete('/balances/999999');
        Assert::assertStatusCode404($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（amount が0）
     */
    public function testBalancePostAmountZero(): void
    {
        $response = $this->request->post('/balances', [
            'amount' => 0,
            'item' => 'テスト',
            'date' => '2021-01-01',
            'kind_element_id' => 2,
            'purpose_element_id' => 2,
            'place_element_id' => 2,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（item が空）
     */
    public function testBalancePostItemEmpty(): void
    {
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => '',
            'date' => '2021-01-01',
            'kind_element_id' => 2,
            'purpose_element_id' => 2,
            'place_element_id' => 2,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（date が不正）
     */
    public function testBalancePostDateInvalid(): void
    {
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'テスト',
            'date' => 'invalid-date',
            'kind_element_id' => 2,
            'purpose_element_id' => 2,
            'place_element_id' => 2,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（kind_element_id が移動ID）
     */
    public function testBalancePostKindElementIdIsMoveId(): void
    {
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'テスト',
            'date' => '2021-01-01',
            'kind_element_id' => 1,
            'purpose_element_id' => 2,
            'place_element_id' => 2,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（purpose_element_id が移動ID）
     */
    public function testBalancePostPurposeElementIdIsMoveId(): void
    {
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'テスト',
            'date' => '2021-01-01',
            'kind_element_id' => 2,
            'purpose_element_id' => 1,
            'place_element_id' => 2,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（place_element_id が移動ID）
     */
    public function testBalancePostPlaceElementIdIsMoveId(): void
    {
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'テスト',
            'date' => '2021-01-01',
            'kind_element_id' => 2,
            'purpose_element_id' => 2,
            'place_element_id' => 1,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 認証なし
     */
    public function testBalanceWithoutAuth(): void
    {
        $noSessionRequest = new Request();

        $response = $noSessionRequest->get('/balances');
        Assert::assertStatusCode401($response->statusCode());
        $response = $noSessionRequest->get('/balances/10');
        Assert::assertStatusCode401($response->statusCode());
        $response = $noSessionRequest->post('/balances', $this->validBalance());
        Assert::assertStatusCode401($response->statusCode());
        $response = $noSessionRequest->put('/balances/10', $this->validBalance());
        Assert::assertStatusCode401($response->statusCode());
        $response = $noSessionRequest->delete('/balances/10');
        Assert::assertStatusCode401($response->statusCode());
    }

    private function validBalance(): array
    {
        return [
            'amount' => -500,
            'item' => 'テスト収支',
            'date' => '2021-05-01',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ];
    }
}
