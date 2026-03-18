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
     * 収支表取得(件数指定)
     */
    public function testBalanceGetWithLimit(): void
    {
        $response = $this->request->get('/balances?limit=20');
        Assert::assertStatusCode200($response->statusCode());
        $balances = $response->jsonBody();
        Assert::assertSame(20, count($balances), '件数指定のカウント');
    }

    /**
     * 収支表取得(並び替え)
     */
    public function testBalanceGetWithOrderBy(): void
    {
        $response = $this->request->get('/balances?orderby=desc');
        Assert::assertStatusCode200($response->statusCode());
        $descFirst = $response->jsonBody()[0];

        // @todo デフォルトで asc
        // $response = $this->request->get('/balances?orderby=asc');
        // Assert::assertStatusCode200($response->statusCode());
        // $ascFirst = $response->jsonBody()[0];

        // desc の先頭 ID が asc の先頭 ID より大きいこと
        // @todo id の順を確認する
        // Assert::assertSame(true, $descFirst['id'] > $ascFirst['id'], '降順の先頭IDが昇順の先頭IDより大きいこと');
    }

    /**
     * 収支表取得(件数指定と並び替え)
     */
    public function testBalanceGetWithLimitAndOrderBy(): void
    {
        $response = $this->request->get('/balances?orderby=desc&limit=20');
        Assert::assertStatusCode200($response->statusCode());
        $balances = $response->jsonBody();
        Assert::assertSame(20, count($balances), '件数指定と並び替えのカウント');
    }

    /**
     * 収支表取得(件数指定パラメータ不正)
     */
    public function testBalanceGetWithInvalidLimit(): void
    {
        $response = $this->request->get('/balances?limit=aaa');
        Assert::assertStatusCode400($response->statusCode());

        $response = $this->request->get('/balances?limit=-1');
        Assert::assertStatusCode400($response->statusCode());

        $response = $this->request->get('/balances?limit=0');
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支表取得(並び替えパラメータ不正)
     */
    public function testBalanceGetWithInvalidOrderBy(): void
    {
        $response = $this->request->get('/balances?orderby=aaa');
        Assert::assertStatusCode400($response->statusCode());
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
     * うるう年テスト
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

        // うるう年でない年の2/29
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
        $balance = $this->validBalance();
        $balance['amount'] = 0;

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（amount がない）
     */
    public function testBalancePostAmountMissing(): void
    {
        $balance = $this->validBalance();
        unset($balance['amount']);

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（amount が文字）
     */
    public function testBalancePostAmountString(): void
    {
        $balance = $this->validBalance();
        $balance['amount'] = '1';

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（item が空）
     */
    public function testBalancePostItemEmpty(): void
    {
        $balance = $this->validBalance();
        $balance['item'] = '';

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（item がない）
     */
    public function testBalancePostItemMissing(): void
    {
        $balance = $this->validBalance();
        unset($balance['item']);

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（date が不正）
     */
    public function testBalancePostDateInvalid(): void
    {
        $balance = $this->validBalance();
        $balance['date'] = 'invalid-date';

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（date がない）
     */
    public function testBalancePostDateMissing(): void
    {
        $balance = $this->validBalance();
        unset($balance['date']);

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（存在しない日付）
     */
    public function testBalancePostDateNotExist(): void
    {
        $balance = $this->validBalance();
        $balance['date'] = '2025-06-31';

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（kind_element_id が移動ID）
     */
    public function testBalancePostKindElementIdIsMoveId(): void
    {
        $balance = $this->validBalance();
        $balance['kind_element_id'] = 1;

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（purpose_element_id が移動ID）
     */
    public function testBalancePostPurposeElementIdIsMoveId(): void
    {
        $balance = $this->validBalance();
        $balance['purpose_element_id'] = 1;

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（place_element_id が移動ID）
     */
    public function testBalancePostPlaceElementIdIsMoveId(): void
    {
        $balance = $this->validBalance();
        $balance['place_element_id'] = 1;

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（外部キー不正）
     */
    public function testBalancePostForeignKeyInvalid(): void
    {
        // kind_element_id が存在しない
        $balance = $this->validBalance();
        $balance['kind_element_id'] = 10000;

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());

        // purpose_element_id が存在しない
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 10000,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // place_element_id が存在しない
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 10000,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（要素パラメータなし）
     */
    public function testBalancePostElementMissing(): void
    {
        // kind_element_id なし
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // purpose_element_id なし
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // place_element_id なし
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支登録バリデーションエラーテスト（要素パラメータが文字列）
     */
    public function testBalancePostElementString(): void
    {
        // kind_element_id が文字列
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 'aaa',
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // purpose_element_id が文字列
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 'aaa',
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // place_element_id が文字列
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 'aaa',
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支更新バリデーションエラーテスト（金額不正）
     */
    public function testBalancePutAmountInvalid(): void
    {
        // 金額が0
        $response = $this->request->put('/balances/10', [
            'amount' => 0,
            'item' => '収入',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // 金額がない
        $response = $this->request->put('/balances/10', [
            'item' => '収入',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // 金額が文字
        $response = $this->request->put('/balances/10', [
            'amount' => 'aaa',
            'item' => '収入',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支更新バリデーションエラーテスト（項目不正）
     */
    public function testBalancePutItemInvalid(): void
    {
        // 項目が空文字列
        $response = $this->request->put('/balances/10', [
            'amount' => 500,
            'item' => '',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // 項目がない
        $response = $this->request->put('/balances/10', [
            'amount' => 500,
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支更新バリデーションエラーテスト（要素が移動ID）
     */
    public function testBalancePutElementIsMoveId(): void
    {
        $response = $this->request->put('/balances/10', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 1,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        $response = $this->request->put('/balances/10', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 1,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        $response = $this->request->put('/balances/10', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 1,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支更新バリデーションエラーテスト（外部キー不正）
     */
    public function testBalancePutForeignKeyInvalid(): void
    {
        $response = $this->request->put('/balances/10', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 10000,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        $response = $this->request->put('/balances/10', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 10000,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        $response = $this->request->put('/balances/10', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 10000,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支更新バリデーションエラーテスト（要素パラメータなし）
     */
    public function testBalancePutElementMissing(): void
    {
        // kind_element_id なし
        $response = $this->request->put('/balances/10', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // purpose_element_id なし
        $response = $this->request->put('/balances/10', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // place_element_id なし
        $response = $this->request->put('/balances/10', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支更新バリデーションエラーテスト（要素パラメータが文字列）
     */
    public function testBalancePutElementString(): void
    {
        // kind_element_id が文字列
        $response = $this->request->put('/balances/10', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 'aaa',
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // purpose_element_id が文字列
        $response = $this->request->put('/balances/10', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 'aaa',
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // place_element_id が文字列
        $response = $this->request->put('/balances/10', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 'aaa',
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支更新バリデーションエラーテスト（日付不正）
     */
    public function testBalancePutDateInvalid(): void
    {
        // 日付なし
        $response = $this->request->put('/balances/10', [
            'amount' => 500,
            'item' => '収入',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // 存在しない日付
        $response = $this->request->put('/balances/10', [
            'amount' => 500,
            'item' => '収入',
            'date' => '2025-06-31',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // うるう年でない年の2/29
        $response = $this->request->put('/balances/10', [
            'amount' => 500,
            'item' => '収入',
            'date' => '2025-02-29',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支更新(存在しない)
     */
    public function testBalancePutNotFound(): void
    {
        // そもそもない
        $response = $this->request->put('/balances/10000', [
            'amount' => 500,
            'item' => '収入',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode404($response->statusCode());

        // 移動レコード
        $response = $this->request->put('/balances/201', [
            'amount' => 500,
            'item' => '収入',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode404($response->statusCode());
    }

    /**
     * 収支削除テスト
     */
    public function testBalanceDelete(): void
    {
        // 登録
        $response = $this->request->post('/balances', $this->validBalance());
        Assert::assertStatusCode200($response->statusCode());
        $id = $response->jsonBody();

        // 削除
        $response = $this->request->delete('/balances/' . $id);
        Assert::assertStatusCode200($response->statusCode());

        // 削除後に取得すると 404 になること
        $response = $this->request->get('/balances/' . $id);
        Assert::assertStatusCode404($response->statusCode());
    }

    /**
     * 収支削除(存在しない)
     */
    public function testBalanceDeleteNotFound(): void
    {
        // そもそもない
        $response = $this->request->delete('/balances/10000');
        Assert::assertStatusCode404($response->statusCode());

        // 移動レコード
        $response = $this->request->delete('/balances/201');
        Assert::assertStatusCode404($response->statusCode());
    }

    /**
     * 収支取得(存在しない)
     */
    public function testBalanceShowNotFoundMoveRecord(): void
    {
        // 移動レコード
        $response = $this->request->get('/balances/201');
        Assert::assertStatusCode404($response->statusCode());
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
