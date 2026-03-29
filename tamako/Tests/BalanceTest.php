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

        $this->assertBalanceFields($response->jsonBody()[0], $this->validBalance(), '一覧取得');

        $balance = $response->jsonBody()[0];
        Assert::assertSame('kind_ele_desc_2', $balance['kind_element_description'], "kind_element_description");
        Assert::assertSame('purpose_ele_desc_3', $balance['purpose_element_description'], "purpose_element_description");
        Assert::assertSame('place_ele_desc_4', $balance['place_element_description'], "place_element_description");
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
        Assert::assertSame('E103', $response->jsonBody()['code'], '順序指定が未定義の文字列');
    }

    /**
     * 収支CRUDテスト
     */
    public function testBalanceCRUD(): void
    {
        $createData = $this->validBalance();
        $updateData = [
            'amount' => -1000,
            'item' => 'テスト収支更新後',
            'date' => '2021-04-01',
            'kind_element_id' => 12,
            'purpose_element_id' => 13,
            'place_element_id' => 14,
        ];

        // 登録
        $response = $this->request->post('/balances', $createData);
        Assert::assertStatusCode200($response->statusCode());
        $this->assertBalanceFields($response->jsonBody(), $createData, '収支登録後');

        $id = $response->jsonBody()['id'];

        // 個別取得
        $response = $this->request->get('/balances/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $this->assertBalanceFields($response->jsonBody(), $createData, '収支登録後');

        $balance = $response->jsonBody();
        Assert::assertSame('kind_ele_desc_2', $balance['kind_element_description'], "kind_element_description");
        Assert::assertSame('purpose_ele_desc_3', $balance['purpose_element_description'], "purpose_element_description");
        Assert::assertSame('place_ele_desc_4', $balance['place_element_description'], "place_element_description");

        // 更新
        $response = $this->request->put('/balances/' . $id, $updateData);
        Assert::assertStatusCode200($response->statusCode());
        $this->assertBalanceFields($response->jsonBody(), $updateData, '更新後');

        // 削除
        $response = $this->request->delete('/balances/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $this->assertBalanceFields($response->jsonBody(), $updateData, '更新後');

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
        $balance = $this->validBalance();
        $balance['date'] = '2024-02-29';
        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode200($response->statusCode());
        Assert::assertSame('2024-02-29', $response->jsonBody()['date'], '閏日の登録');

        $id = $response->jsonBody()['id'];

        // 更新
        $balance = $this->validBalance();
        $balance['date'] = '2020-02-29';
        $balance['kind_element_id'] = 12;
        $balance['purpose_element_id'] = 13;
        $balance['place_element_id'] = 14;
        $balance['amount'] = -1000;
        $balance['item'] = 'テスト収支更新後';
        $response = $this->request->put('/balances/' . $id, $balance);
        Assert::assertStatusCode200($response->statusCode());
        Assert::assertSame('2020-02-29', $response->jsonBody()['date'], '閏日の更新');

        // 異常系

        // うるう年でない年の2/29
        $balance = $this->validBalance();
        $balance['date'] = '2023-02-29';
        $response = $this->request->post('/balances', $balance);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'うるう年ではない年のうるう日登録');

        // 更新
        $balance = $this->validBalance();
        $balance['date'] = '2022-02-29';
        $balance['kind_element_id'] = 12;
        $balance['purpose_element_id'] = 13;
        $balance['place_element_id'] = 14;
        $balance['amount'] = -1000;
        $balance['item'] = 'テスト収支更新後';
        $response = $this->request->put('/balances/' . $id, $balance);
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
     * 収支登録バリデーションエラーテスト（amount）
     */
    public function testBalancePostAmountInvalid(): void
    {
        // amount が0
        $this->assertPostError(['amount' => 0], 400, 'E102', 'amountが0');

        // amount がない
        $this->assertPostErrorUnset('amount', 400, 'E109', 'amountがない');

        // amount が null
        $this->assertPostError(['amount' => null], 400, 'E101', 'amountがnull');

        // amount が空文字列
        $this->assertPostError(['amount' => ''], 400, 'E101', 'amountが空文字列');

        // amount が文字列
        $this->assertPostError(['amount' => 'aaa'], 400, 'E101', 'amountが文字列');

        // amount が文字列数字
        $this->assertPostError(['amount' => '1'], 400, 'E101', 'amountが文字列数字');
    }

    /**
     * 収支登録バリデーションエラーテスト（item）
     */
    public function testBalancePostItemInvalid(): void
    {
        // item が空
        $this->assertPostError(['item' => ''], 400, 'E105', 'itemが空');

        // item が null
        $this->assertPostError(['item' => null], 400, 'E101', 'itemがnull');

        // item がない
        $this->assertPostErrorUnset('item', 400, 'E109', 'itemがない');
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
        $this->assertPostError(
            ['item' => 'あいうえおかきくけこさしすせそたちつてとなにぬねのあいうえおかきくけこさしすせそたちつてとなにぬねのあ'],
            400,
            'E105',
            'item長い'
        );
    }

    /**
     * 収支登録バリデーションエラーテスト（date）
     */
    public function testBalancePostDateInvalid(): void
    {
        // 日付が不正
        $this->assertPostError(['date' => 'invalid-date'], 400, 'E103', '日付が不正');

        // 日付がない
        $this->assertPostErrorUnset('date', 400, 'E109', '日付がない');

        // 日付が null
        $this->assertPostError(['date' => null], 400, 'E101', '日付がnull');

        // 存在しない日付
        $this->assertPostError(['date' => '2025-06-31'], 400, 'E103', '存在しない日付');
    }

    /**
     * 収支登録バリデーションエラーテスト（要素が移動ID）
     */
    public function testBalancePostElementIsMoveId(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPostError([$field => 1], 400, 'E108', "{$field}にmove idを使っている");
        }
    }

    /**
     * 収支登録バリデーションエラーテスト（外部キー不正）
     */
    public function testBalancePostForeignKeyInvalid(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPostError([$field => 10000], 409, 'E302', "{$field}がない");
        }
    }

    /**
     * 収支登録バリデーションエラーテスト（要素パラメータなし）
     */
    public function testBalancePostElementMissing(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPostErrorUnset($field, 400, 'E109', "{$field}が空");
        }
    }

    /**
     * 収支登録バリデーションエラーテスト（要素パラメータが null）
     */
    public function testBalancePostElementNull(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPostError([$field => null], 400, 'E101', "{$field}がnull");
        }
    }

    /**
     * 収支登録バリデーションエラーテスト（要素パラメータが文字列）
     */
    public function testBalancePostElementString(): void
    {
        $elementValues = $this->elementIdValues();

        foreach ($this->elementIdFields() as $field) {
            // 空文字列
            $this->assertPostError([$field => ''], 400, 'E101', "{$field}が空文字列");

            // 文字列
            $this->assertPostError([$field => 'aaa'], 400, 'E101', "{$field}が文字列");

            // 文字列数字
            $this->assertPostError([$field => (string) $elementValues[$field]], 400, 'E101', "{$field}が文字列数字");
        }
    }

    /**
     * 収支更新バリデーションエラーテスト（金額不正）
     */
    public function testBalancePutAmountInvalid(): void
    {
        // 金額が0
        $this->assertPutError(['amount' => 0], 400, 'E102', 'amountが0');

        // 金額がnull
        $this->assertPutError(['amount' => null], 400, 'E101', 'amountがnull');

        // 金額がない
        $this->assertPutErrorUnset('amount', 400, 'E109', 'amountがない');

        // 金額が空文字列
        $this->assertPutError(['amount' => ''], 400, 'E101', 'amountが文字列');

        // 金額が文字列
        $this->assertPutError(['amount' => 'aaa'], 400, 'E101', 'amountが文字列');

        // 金額が文字列数字
        $this->assertPutError(['amount' => '100'], 400, 'E101', 'amountが文字列');
    }

    /**
     * 収支更新バリデーションエラーテスト（項目不正）
     */
    public function testBalancePutItemInvalid(): void
    {
        // 項目が空文字列
        $this->assertPutError(['item' => ''], 400, 'E105', 'itemが空文字列');

        // 項目がnull
        $this->assertPutError(['item' => null], 400, 'E101', 'itemがnull');

        // 項目がない
        $this->assertPutErrorUnset('item', 400, 'E109', 'itemがない');
    }

    /**
     * 収支更新バリデーションエラーテスト（要素が移動ID）
     */
    public function testBalancePutElementIsMoveId(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPutError([$field => 1], 400, 'E108', "{$field}が移動ID");
        }
    }

    /**
     * 収支更新バリデーションエラーテスト（外部キー不正）
     */
    public function testBalancePutForeignKeyInvalid(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPutError([$field => 10000], 409, 'E302', "{$field}がない");
        }
    }

    /**
     * 収支更新バリデーションエラーテスト（要素パラメータなし）
     */
    public function testBalancePutElementMissing(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPutErrorUnset($field, 400, 'E109', "{$field}が空");
        }
    }

    /**
     * 収支更新バリデーションエラーテスト（要素パラメータが null）
     */
    public function testBalancePutElementNull(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPutError([$field => null], 400, 'E101', "{$field}がnull");
        }
    }

    /**
     * 収支更新バリデーションエラーテスト（要素パラメータが文字列）
     */
    public function testBalancePutElementString(): void
    {
        $elementValues = $this->elementIdValues();

        foreach ($this->elementIdFields() as $field) {
            // 空文字列
            $this->assertPutError([$field => ''], 400, 'E101', "{$field}が空文字列");

            // 文字列
            $this->assertPutError([$field => 'aaa'], 400, 'E101', "{$field}が文字列");

            // 文字列数字
            $this->assertPutError([$field => (string) $elementValues[$field]], 400, 'E101', "{$field}が文字列数字");
        }
    }

    /**
     * 収支更新バリデーションエラーテスト（日付不正）
     */
    public function testBalancePutDateInvalid(): void
    {
        // 日付なし
        $this->assertPutErrorUnset('date', 400, 'E109', 'dateがない');

        // null 日付
        $this->assertPutError(['date' => null], 400, 'E101', 'dateがnull');

        // 存在しない日付
        $this->assertPutError(['date' => '2025-06-31'], 400, 'E103', 'dateがない');

        // うるう年でない年の2/29
        $this->assertPutError(['date' => '2025-02-29'], 400, 'E103', 'うるう年ではないうるう日');
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
        foreach (['purposes', 'places'] as $moveType) {
            // 移動レコード登録
            $response = $this->request->post('/moves/' . $moveType, [
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
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 移動レコードは存在しない扱い");
            $response = $this->request->get('/balances/' . $afterRecordId);
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 移動レコードは存在しない扱い");

            // 更新
            $balance = $this->validBalance();
            $response = $this->request->put('/balances/' . $beforeRecordId, $balance);
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 移動レコードは存在しない扱い");
            $response = $this->request->put('/balances/' . $afterRecordId, $balance);
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 移動レコードは存在しない扱い");

            // 削除
            $response = $this->request->delete('/balances/' . $beforeRecordId, $balance);
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 移動レコードは存在しない扱い");
            $response = $this->request->delete('/balances/' . $afterRecordId, $balance);
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 移動レコードは存在しない扱い");
        }
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

    /**
     * 要素IDフィールド名の一覧
     */
    private function elementIdFields(): array
    {
        return ['kind_element_id', 'purpose_element_id', 'place_element_id'];
    }

    /**
     * validBalance の要素IDの値マップ
     */
    private function elementIdValues(): array
    {
        return [
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ];
    }

    /**
     * 収支の各フィールドをアサートする
     */
    private function assertBalanceFields(array $balance, array $expected, string $prefix): void
    {
        Assert::assertSame($expected['amount'], $balance['amount'], "{$prefix}の amount");
        Assert::assertSame($expected['item'], $balance['item'], "{$prefix}の item");
        Assert::assertSame($expected['date'], $balance['date'], "{$prefix}の date");
        Assert::assertSame($expected['kind_element_id'], $balance['kind_element_id'], "{$prefix}の kind_element_id");
        Assert::assertSame($expected['purpose_element_id'], $balance['purpose_element_id'], "{$prefix}の purpose_element_id");
        Assert::assertSame($expected['place_element_id'], $balance['place_element_id'], "{$prefix}の place_element_id");
    }

    /**
     * POST でバリデーションエラーをアサートする（フィールド上書き）
     */
    private function assertPostError(array $overrides, int $statusCode, string $errorCode, string $message): void
    {
        $balance = array_merge($this->validBalance(), $overrides);
        $response = $this->request->post('/balances', $balance);
        $this->assertErrorResponse($response, $statusCode, $errorCode, $message);
    }

    /**
     * POST でバリデーションエラーをアサートする（フィールド削除）
     */
    private function assertPostErrorUnset(string $field, int $statusCode, string $errorCode, string $message): void
    {
        $balance = $this->validBalance();
        unset($balance[$field]);
        $response = $this->request->post('/balances', $balance);
        $this->assertErrorResponse($response, $statusCode, $errorCode, $message);
    }

    /**
     * PUT でバリデーションエラーをアサートする（フィールド上書き）
     */
    private function assertPutError(array $overrides, int $statusCode, string $errorCode, string $message): void
    {
        $balance = array_merge($this->validBalance(), $overrides);
        $response = $this->request->put('/balances/10', $balance);
        $this->assertErrorResponse($response, $statusCode, $errorCode, $message);
    }

    /**
     * PUT でバリデーションエラーをアサートする（フィールド削除）
     */
    private function assertPutErrorUnset(string $field, int $statusCode, string $errorCode, string $message): void
    {
        $balance = $this->validBalance();
        unset($balance[$field]);
        $response = $this->request->put('/balances/10', $balance);
        $this->assertErrorResponse($response, $statusCode, $errorCode, $message);
    }

    /**
     * エラーレスポンスをアサートする
     */
    private function assertErrorResponse($response, int $statusCode, string $errorCode, string $message): void
    {
        $assertMethod = 'assertStatusCode' . $statusCode;
        Assert::$assertMethod($response->statusCode());
        Assert::assertSame($errorCode, $response->jsonBody()['code'], $message);
    }
}
