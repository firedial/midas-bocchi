<?php

require_once __DIR__ . '/../TestRunner/TestCase.php';

class BalanceTest extends TestCase
{
    /**
     * 収支一覧取得テスト(内容)
     */
    public function testBalanceGetResponseBody(): void
    {
        $response = $this->request->post('/balances', $this->validBalance());
        Assert::assertStatusCode200($response->statusCode());

        $response = $this->request->get('/balances?orderby=desc');
        Assert::assertStatusCode200($response->statusCode());

        $balance = $response->jsonBody()[0];
        Assert::assertSame(-500, $balance['amount'], '一覧取得の amount');
        Assert::assertSame('テスト収支', $balance['item'], '一覧取得の item');
        Assert::assertSame('2021-05-01', $balance['date'], '一覧取得の date');
        Assert::assertSame(2, $balance['kind_element_id'], '一覧取得の kind_element_id');
        Assert::assertSame(3, $balance['purpose_element_id'], '一覧取得の purpose_element_id');
        Assert::assertSame(4, $balance['place_element_id'], '一覧取得の place_element_id');
    }

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

        $balances = $response->jsonBody();
        $beforeId = $response->jsonBody()[0]['id'] + 1;

        foreach ($balances as $balance) {
            Assert::assertLT($balance['id'], $beforeId, '順序の指定');
            $beforeId = $balance['id'];
        }
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

        $beforeId = $response->jsonBody()[0]['id'] + 1;

        foreach ($balances as $balance) {
            Assert::assertLT($balance['id'], $beforeId, '順序の指定');
            $beforeId = $balance['id'];
        }
    }

    /**
     * 収支表取得(件数指定パラメータ不正)
     */
    public function testBalanceGetWithInvalidLimit(): void
    {
        $response = $this->request->get('/balances?limit=aaa');
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], '件数指定が文字列');

        $response = $this->request->get('/balances?limit=-1');
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E102', $response->jsonBody()['code'], '件数指定がマイナス');

        $response = $this->request->get('/balances?limit=0');
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E102', $response->jsonBody()['code'], '件数指定が0');
    }

    /**
     * 収支表取得(並び替えパラメータ不正)
     */
    public function testBalanceGetWithInvalidOrderBy(): void
    {
        $response = $this->request->get('/balances?orderby=aaa');
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E106', $response->jsonBody()['code'], '順序指定が未定義の文字列');
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
        $balance = $response->jsonBody();
        Assert::assertSame(-500, $balance['amount'], '収支登録後の amount');
        Assert::assertSame('テスト収支', $balance['item'], '収支登録後の item');
        Assert::assertSame('2021-05-01', $balance['date'], '収支登録後の date');
        Assert::assertSame(2, $balance['kind_element_id'], '収支登録後の kind_element_id');
        Assert::assertSame(3, $balance['purpose_element_id'], '収支登録後の purpose_element_id');
        Assert::assertSame(4, $balance['place_element_id'], '収支登録後の place_element_id');

        $id = $balance['id'];

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
        $balance = $response->jsonBody();
        Assert::assertSame(-1000, $balance['amount'], '更新後の amount');
        Assert::assertSame('テスト収支更新後', $balance['item'], '更新後の item');
        Assert::assertSame('2021-04-01', $balance['date'], '更新後の date');
        Assert::assertSame(12, $balance['kind_element_id'], '更新後の kind_element_id');
        Assert::assertSame(13, $balance['purpose_element_id'], '更新後の purpose_element_id');
        Assert::assertSame(14, $balance['place_element_id'], '更新後の place_element_id');

        // 削除後に取得すると 404 になること
        $response = $this->request->get('/balances/' . $id);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '削除後の取得');
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
        $balance = $response->jsonBody();
        Assert::assertSame('2024-02-29', $balance['date'], '閏日の登録');

        $id = $balance['id'];

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
        Assert::assertSame('E103', $response->jsonBody()['code'], 'うるう年ではない年のうるう日登録');

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
        Assert::assertSame('E103', $response->jsonBody()['code'], 'うるう年ではない年のうるう日更新');
    }

    /**
     * 存在しない収支テスト
     */
    public function testBalanceShowNotFound(): void
    {
        $response = $this->request->get('/balances/999999');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '取得');

        $response = $this->request->put('/balances/999999', $this->validBalance());
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '更新');

        $response = $this->request->delete('/balances/999999');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '削除');
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
        Assert::assertSame('E106', $response->jsonBody()['code'], 'amountが0');
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
        Assert::assertSame('E109', $response->jsonBody()['code'], 'amountがない');
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
        Assert::assertSame('E101', $response->jsonBody()['code'], 'amountが文字列');
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
        Assert::assertSame('E109', $response->jsonBody()['code'], 'itemが空');
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
        Assert::assertSame('E109', $response->jsonBody()['code'], 'itemがない');
    }

    /**
     * 収支登録バリデーションエラーテスト（item の長さ）
     */
    public function testBalancePostItemLength(): void
    {
        // 50文字
        $balance = $this->validBalance();
        $balance['item'] = 'あいうえおかきくけこさしすせそたちつてとなにぬねのあいうえおかきくけこさしすせそたちつてとなにぬねの';
        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode200($response->statusCode());

        // 51文字
        $balance = $this->validBalance();
        $balance['item'] = 'あいうえおかきくけこさしすせそたちつてとなにぬねのあいうえおかきくけこさしすせそたちつてとなにぬねのあ';
        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E105', $response->jsonBody()['code'], 'item長い');
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
        Assert::assertSame('E103', $response->jsonBody()['code'], '日付が不正');
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
        Assert::assertSame('E109', $response->jsonBody()['code'], '日付がない');
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
        Assert::assertSame('E103', $response->jsonBody()['code'], '存在しない日付');
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
        Assert::assertSame('E108', $response->jsonBody()['code'], 'kindにmove idを使っている');
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
        Assert::assertSame('E108', $response->jsonBody()['code'], 'purposeにmove idを使っている');
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
        Assert::assertSame('E108', $response->jsonBody()['code'], 'placeにmove idを使っている');
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
        Assert::assertSame('E302', $response->jsonBody()['code'], 'kind idがない');

        // purpose_element_id が存在しない
        $balance = $this->validBalance();
        $balance['purpose_element_id'] = 10000;

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E302', $response->jsonBody()['code'], 'purpose idがない');

        // place_element_id が存在しない
        $balance = $this->validBalance();
        $balance['place_element_id'] = 10000;

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E302', $response->jsonBody()['code'], 'place idがない');
    }

    /**
     * 収支登録バリデーションエラーテスト（要素パラメータなし）
     */
    public function testBalancePostElementMissing(): void
    {
        // kind_element_id なし
        $balance = $this->validBalance();
        unset($balance['kind_element_id']);

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'kind idが空');

        // kind_element_id なし
        $balance = $this->validBalance();
        unset($balance['purpose_element_id']);

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'purpose idが空');

        // place_element_id なし
        $balance = $this->validBalance();
        unset($balance['place_element_id']);

        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'place idが空');
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

        // kind_element_id が文字列数字
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => '2',
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // purpose_element_id が文字列数字
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => '3',
            'place_element_id' => 4,
        ]);
        Assert::assertStatusCode400($response->statusCode());

        // place_element_id が文字列数字
        $response = $this->request->post('/balances', [
            'amount' => -500,
            'item' => 'うどん',
            'date' => '2024-10-23',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => '4',
        ]);
        Assert::assertStatusCode400($response->statusCode());
    }

    /**
     * 収支更新バリデーションエラーテスト（金額不正）
     */
    public function testBalancePutAmountInvalid(): void
    {
        // 金額が0
        $balance = $this->validBalance();
        $balance['amount'] = 0;
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E106', $response->jsonBody()['code'], 'amountが0');

        // 金額がない
        $balance = $this->validBalance();
        unset($balance['amount']);
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'amountがない');

        // 金額が文字
        $balance = $this->validBalance();
        $balance['amount'] = 'aaa';
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'amountが文字列');

        // 金額が文字数字
        $balance = $this->validBalance();
        $balance['amount'] = '100';
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'amountが文字列');
    }

    /**
     * 収支更新バリデーションエラーテスト（項目不正）
     */
    public function testBalancePutItemInvalid(): void
    {
        // 項目が空文字列
        $balance = $this->validBalance();
        $balance['item'] = '';
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'itemが空文字列');

        // 項目がない
        $balance = $this->validBalance();
        unset($balance['item']);
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'itemがない');
    }

    /**
     * 収支更新バリデーションエラーテスト（要素が移動ID）
     */
    public function testBalancePutElementIsMoveId(): void
    {
        $balance = $this->validBalance();
        $balance['kind_element_id'] = 1;
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E108', $response->jsonBody()['code'], 'kind idが移動ID');

        $balance = $this->validBalance();
        $balance['purpose_element_id'] = 1;
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E108', $response->jsonBody()['code'], 'purpose idが移動ID');

        $balance = $this->validBalance();
        $balance['place_element_id'] = 1;
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E108', $response->jsonBody()['code'], 'place idが移動ID');
    }

    /**
     * 収支更新バリデーションエラーテスト（外部キー不正）
     */
    public function testBalancePutForeignKeyInvalid(): void
    {
        $balance = $this->validBalance();
        $balance['kind_element_id'] = 10000;
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E302', $response->jsonBody()['code'], 'kind idがない');

        $balance = $this->validBalance();
        $balance['purpose_element_id'] = 10000;
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E302', $response->jsonBody()['code'], 'purpose idがない');

        $balance = $this->validBalance();
        $balance['place_element_id'] = 10000;
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E302', $response->jsonBody()['code'], 'place idがない');
    }

    /**
     * 収支更新バリデーションエラーテスト（要素パラメータなし）
     */
    public function testBalancePutElementMissing(): void
    {
        $balance = $this->validBalance();
        unset($balance['kind_element_id']);
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'kind idが空');

        $balance = $this->validBalance();
        unset($balance['purpose_element_id']);
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'purpose idが空');

        $balance = $this->validBalance();
        unset($balance['place_element_id']);
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'place idが空');
    }

    /**
     * 収支更新バリデーションエラーテスト（要素パラメータが文字列）
     */
    public function testBalancePutElementString(): void
    {
        $balance = $this->validBalance();
        $balance['kind_element_id'] = 'aaa';
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'kind idが空');

        $balance = $this->validBalance();
        $balance['purpose_element_id'] = 'aaa';
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'purpose idが空');

        $balance = $this->validBalance();
        $balance['place_element_id'] = 'aaa';
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'place idが空');
    }

    /**
     * 収支更新バリデーションエラーテスト（日付不正）
     */
    public function testBalancePutDateInvalid(): void
    {
        // 日付なし
        $balance = $this->validBalance();
        unset($balance['date']);
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'dateがない');

        // 存在しない日付
        $balance = $this->validBalance();
        $balance['date'] = '2025-06-31';
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'dateがない');

        // うるう年でない年の2/29
        $balance = $this->validBalance();
        $balance['date'] = '2025-02-29';
        $response = $this->request->put('/balances/10', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'うるう年ではないうるう日');
    }

    /**
     * 収支更新(存在しない)
     */
    public function testBalancePutNotFound(): void
    {
        // そもそもない
        $balance = $this->validBalance();
        $response = $this->request->put('/balances/10000', $balance);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '更新対象のレコードがない');
    }

    /**
     * 収支削除テスト
     */
    public function testBalanceDelete(): void
    {
        // 登録
        $response = $this->request->post('/balances', $this->validBalance());
        Assert::assertStatusCode200($response->statusCode());
        $id = $response->jsonBody()['id'];

        // 削除
        $response = $this->request->delete('/balances/' . $id);
        Assert::assertStatusCode200($response->statusCode());

        // 削除後に取得すると 404 になること
        $response = $this->request->get('/balances/' . $id);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '削除対象のレコードがない');
    }

    /**
     * 収支削除(存在しない)
     */
    public function testBalanceDeleteNotFound(): void
    {
        // そもそもない
        $response = $this->request->delete('/balances/10000');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '削除対象のレコードがない');
    }

    /**
     * 認証なし
     */
    public function testBalanceWithoutAuth(): void
    {
        $noSessionRequest = new Request();

        $response = $noSessionRequest->get('/balances');
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し一覧取得');

        $response = $noSessionRequest->get('/balances/10');
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し取得');

        $response = $noSessionRequest->post('/balances', $this->validBalance());
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し登録');

        $response = $noSessionRequest->put('/balances/10', $this->validBalance());
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し更新');

        $response = $noSessionRequest->delete('/balances/10');
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し削除');
    }

    /**
     * 移動レコードの操作
     */
    public function testBalanceMove(): void
    {
        // purpose
        // 移動レコード登録
        $response = $this->request->post('/moves/purposes', [
            'amount' => 500,
            'item' => 'テスト',
            'before_id' => 2,
            'after_id' => 5,
            'date' => '2025-03-06',
        ]);
        $move = $response->jsonBody();
        Assert::assertStatusCode200($response->statusCode());

        $beforeRecordId = $move['id'];
        $afterRecordId = $beforeRecordId + 1;

        // 取得
        $response = $this->request->get('/balances/' . $beforeRecordId);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '移動レコードは存在しない扱い');
        $response = $this->request->get('/balances/' . $afterRecordId);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '移動レコードは存在しない扱い');

        // 更新
        $balance = $this->validBalance();
        $response = $this->request->put('/balances/' . $beforeRecordId, $balance);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '移動レコードは存在しない扱い');
        $response = $this->request->put('/balances/' . $afterRecordId, $balance);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '移動レコードは存在しない扱い');

        // 削除
        $response = $this->request->delete('/balances/' . $beforeRecordId, $balance);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '移動レコードは存在しない扱い');
        $response = $this->request->delete('/balances/' . $afterRecordId, $balance);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '移動レコードは存在しない扱い');

        // place
        // 移動レコード登録
        $response = $this->request->post('/moves/places', [
            'amount' => 500,
            'item' => 'テスト',
            'before_id' => 2,
            'after_id' => 5,
            'date' => '2025-03-06',
        ]);
        $move = $response->jsonBody();
        Assert::assertStatusCode200($response->statusCode());

        $beforeRecordId = $move['id'];
        $afterRecordId = $beforeRecordId + 1;

        // 取得
        $response = $this->request->get('/balances/' . $beforeRecordId);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '移動レコードは存在しない扱い');
        $response = $this->request->get('/balances/' . $afterRecordId);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '移動レコードは存在しない扱い');

        // 更新
        $balance = $this->validBalance();
        $response = $this->request->put('/balances/' . $beforeRecordId, $balance);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '移動レコードは存在しない扱い');
        $response = $this->request->put('/balances/' . $afterRecordId, $balance);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '移動レコードは存在しない扱い');

        // 削除
        $response = $this->request->delete('/balances/' . $beforeRecordId, $balance);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '移動レコードは存在しない扱い');
        $response = $this->request->delete('/balances/' . $afterRecordId, $balance);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '移動レコードは存在しない扱い');
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
