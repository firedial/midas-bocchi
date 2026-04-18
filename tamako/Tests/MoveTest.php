<?php

require_once __DIR__ . '/../TestRunner/TestCase.php';

class MoveTest extends TestCase
{
    /**
     * 移動一覧取得テスト(内容) - purposes
     */
    public function testMoveGetPurposesResponseBody(): void
    {
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());

        $response = $this->request->get('/moves/purposes');
        Assert::assertStatusCode200($response->statusCode());

        $this->assertMoveFields($response->jsonBody()[0], $this->validMove(), '一覧取得');

        $move = $response->jsonBody()[0];
        Assert::assertSame('purpose_ele_desc_2', $move['before_description'], "purpose の before_description");
        Assert::assertSame('purpose_ele_desc_3', $move['after_description'], "purpose の after_description");
    }

    /**
     * 移動一覧取得テスト(内容) - places
     */
    public function testMoveGetPlacesResponseBody(): void
    {
        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());

        $response = $this->request->get('/moves/places');
        Assert::assertStatusCode200($response->statusCode());

        $this->assertMoveFields($response->jsonBody()[0], $this->validMove(), '一覧取得');

        $move = $response->jsonBody()[0];
        Assert::assertSame('place_ele_desc_2', $move['before_description'], "place の before_description");
        Assert::assertSame('place_ele_desc_3', $move['after_description'], "place の after_description");
    }

    /**
     * 移動一覧取得テスト(属性名不正)
     */
    public function testMoveGetInvalidAttributeName(): void
    {
        $response = $this->request->get('/moves/aaa');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E305', $response->jsonBody()['code'], '属性名不正');
    }

    /**
     * 移動CRUDテスト - purposes
     */
    public function testMoveCRUDPurposes(): void
    {
        $this->assertMoveCRUD('purposes');
    }

    /**
     * 移動CRUDテスト - places
     */
    public function testMoveCRUDPlaces(): void
    {
        $this->assertMoveCRUD('places');
    }

    /**
     * うるう年テスト
     */
    public function testMoveLeapDay(): void
    {
        // 正常系
        foreach ($this->moveTypes() as $moveType) {
            $move = $this->validMove();
            $move['date'] = '2024-02-29';
            $response = $this->request->post('/moves/' . $moveType, $move);
            Assert::assertStatusCode200($response->statusCode());
            Assert::assertSame('2024-02-29', $response->jsonBody()['date'], "閏日の登録 {$moveType}");

            $id = $response->jsonBody()['id'];

            // 更新
            $move = $this->validMove();
            $move['date'] = '2020-02-29';
            $response = $this->request->put('/moves/' . $moveType . '/' . $id, $move);
            Assert::assertStatusCode200($response->statusCode());
            Assert::assertSame('2020-02-29', $response->jsonBody()['date'], "閏日の更新 {$moveType}");
        }

        // 異常系
        foreach ($this->moveTypes() as $moveType) {
            // うるう年でない年の2/29
            $move = $this->validMove();
            $move['date'] = '2023-02-29';
            $response = $this->request->post('/moves/' . $moveType, $move);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E103', $response->jsonBody()['code'], "うるう年ではない年のうるう日登録 {$moveType}");
        }

        // 更新の異常系（正常系で作成したレコードを利用）
        foreach ($this->moveTypes() as $moveType) {
            $move = $this->validMove();
            $move['date'] = '2024-02-29';
            $response = $this->request->post('/moves/' . $moveType, $move);
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $move = $this->validMove();
            $move['date'] = '2022-02-29';
            $response = $this->request->put('/moves/' . $moveType . '/' . $id, $move);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E103', $response->jsonBody()['code'], "うるう年ではない年のうるう日更新 {$moveType}");
        }
    }

    /**
     * 存在しない移動テスト
     */
    public function testMoveShowNotFound(): void
    {
        foreach ($this->moveTypes() as $moveType) {
            $response = $this->request->get('/moves/' . $moveType . '/999999');
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 取得");

            $response = $this->request->put('/moves/' . $moveType . '/999999', $this->validMove());
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 更新");

            $response = $this->request->delete('/moves/' . $moveType . '/999999');
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 削除");
        }
    }

    /**
     * 移動登録バリデーションエラーテスト（金額が負）
     */
    public function testMovePostAmountNegative(): void
    {
        $this->assertPostErrorForAll(['amount' => -500], 400, 'E102', 'amountが負');
    }

    /**
     * 移動登録バリデーションエラーテスト（金額が0）
     */
    public function testMovePostAmountZero(): void
    {
        $this->assertPostErrorForAll(['amount' => 0], 400, 'E102', 'amountが0');
    }

    /**
     * 移動登録バリデーションエラーテスト（金額がない）
     */
    public function testMovePostAmountMissing(): void
    {
        $this->assertPostErrorUnsetForAll('amount', 400, 'E109', 'amountがない');
    }

    /**
     * 移動登録バリデーションエラーテスト（金額が null）
     */
    public function testMovePostAmountNull(): void
    {
        $this->assertPostErrorForAll(['amount' => null], 400, 'E101', 'amountがnull');
    }

    /**
     * 移動登録バリデーションエラーテスト（金額が文字）
     */
    public function testMovePostAmountString(): void
    {
        $this->assertPostErrorForAll(['amount' => ''], 400, 'E101', 'amountが空文字列');
        $this->assertPostErrorForAll(['amount' => 'aaa'], 400, 'E101', 'amountが文字列');
        $this->assertPostErrorForAll(['amount' => '100'], 400, 'E101', 'amountが文字列数字');
    }

    /**
     * 移動登録バリデーションエラーテスト（項目が空）
     */
    public function testMovePostItemEmpty(): void
    {
        $this->assertPostErrorForAll(['item' => ''], 400, 'E105', 'itemが空');
    }

    /**
     * 移動登録バリデーションエラーテスト（項目が null）
     */
    public function testMovePostItemNull(): void
    {
        $this->assertPostErrorForAll(['item' => null], 400, 'E101', 'itemがnull');
    }

    /**
     * 移動登録バリデーションエラーテスト（項目がない）
     */
    public function testMovePostItemMissing(): void
    {
        $this->assertPostErrorUnsetForAll('item', 400, 'E109', 'itemがない');
    }

    /**
     * 移動登録バリデーションエラーテスト（項目の長さ）
     */
    public function testMovePostItemLength(): void
    {
        foreach ($this->moveTypes() as $moveType) {
            // 50文字
            $move = $this->validMove();
            $move['item'] = 'あいうえおかきくけこさしすせそたちつてとなにぬねのあいうえおかきくけこさしすせそたちつてとなにぬねの';
            $response = $this->request->post('/moves/' . $moveType, $move);
            Assert::assertStatusCode200($response->statusCode());
        }

        // 51文字
        $this->assertPostErrorForAll(
            ['item' => 'あいうえおかきくけこさしすせそたちつてとなにぬねのあいうえおかきくけこさしすせそたちつてとなにぬねのあ'],
            400,
            'E105',
            'item長い'
        );
    }

    /**
     * 移動登録バリデーションエラーテスト（要素が移動ID）
     */
    public function testMovePostElementIsMoveId(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPostErrorForAll([$field => 1], 400, 'E108', "{$field}が移動ID");
        }
    }

    /**
     * 移動登録バリデーションエラーテスト（外部キー不正）
     */
    public function testMovePostForeignKeyInvalid(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPostErrorForAll([$field => 10000], 409, 'E302', "{$field}がない");
        }
    }

    /**
     * 移動登録バリデーションエラーテスト（要素パラメータなし）
     */
    public function testMovePostElementMissing(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPostErrorUnsetForAll($field, 400, 'E109', "{$field}が空");
        }
    }

    /**
     * 移動登録バリデーションエラーテスト（要素パラメータ null）
     */
    public function testMovePostElementNull(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPostErrorForAll([$field => null], 400, 'E101', "{$field}がnull");
        }
    }

    /**
     * 移動登録バリデーションエラーテスト（要素パラメータが文字列）
     */
    public function testMovePostElementString(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPostErrorForAll([$field => ''], 400, 'E101', "{$field}が空文字列");
            $this->assertPostErrorForAll([$field => 'aaa'], 400, 'E101', "{$field}が文字列");
            $this->assertPostErrorForAll([$field => '5'], 400, 'E101', "{$field}が文字列数字");
        }
    }

    /**
     * 移動登録バリデーションエラーテスト（移動前後で同じ）
     */
    public function testMovePostSameBeforeAfter(): void
    {
        $this->assertPostErrorForAll(['before_id' => 2, 'after_id' => 2], 400, 'E107', '移動前後が同じ');
    }

    /**
     * 移動登録バリデーションエラーテスト（日付不正）
     */
    public function testMovePostDateInvalid(): void
    {
        $this->assertPostErrorForAll(['date' => 'invalid-date'], 400, 'E103', '日付が不正');
    }

    /**
     * 移動登録バリデーションエラーテスト（日付がない）
     */
    public function testMovePostDateMissing(): void
    {
        $this->assertPostErrorUnsetForAll('date', 400, 'E109', '日付がない');
    }

    /**
     * 移動登録バリデーションエラーテスト（日付が null）
     */
    public function testMovePostDateNull(): void
    {
        $this->assertPostErrorForAll(['date' => null], 400, 'E101', '日付がnull');
    }

    /**
     * 移動登録バリデーションエラーテスト（存在しない日付）
     */
    public function testMovePostDateNotExist(): void
    {
        $this->assertPostErrorForAll(['date' => '2024-06-31'], 400, 'E103', '存在しない日付');
    }

    /**
     * 移動取得テスト（属性名不正）
     */
    public function testMoveShowInvalidAttributeName(): void
    {
        $response = $this->request->get('/moves/hoge/232');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E305', $response->jsonBody()['code'], '属性名が存在しない');
    }

    /**
     * 移動取得テスト（収支レコード）
     */
    public function testMoveShowBalanceRecord(): void
    {
        $response = $this->request->get('/balances?limit=1');
        $id = $response->jsonBody()[0]['id'];

        foreach ($this->moveTypes() as $moveType) {
            $response = $this->request->get('/moves/' . $moveType . '/' . $id);
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 収支レコード");
        }
    }

    /**
     * 移動取得テスト（別属性の移動）
     */
    public function testMoveShowDifferentAttribute(): void
    {
        // purposes で登録した移動を places で取得
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->get('/moves/places/' . $purposeId);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposesの移動をplacesで取得');

        // places で登録した移動を purposes で取得
        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        $response = $this->request->get('/moves/purposes/' . $placeId);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'placesの移動をpurposesで取得');
    }

    /**
     * 移動取得テスト（前後のID）
     */
    public function testMoveShowAdjacentIds(): void
    {
        foreach ($this->moveTypes() as $moveType) {
            $response = $this->request->post('/moves/' . $moveType, $this->validMove());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $response = $this->request->get('/moves/' . $moveType . '/' . ($id - 1));
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 前のID");

            $response = $this->request->get('/moves/' . $moveType . '/' . ($id + 1));
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 後のID");
        }
    }

    /**
     * 移動更新バリデーションエラーテスト（金額が負）
     */
    public function testMovePutAmountNegative(): void
    {
        $this->assertPutErrorForAll(['amount' => -500], 400, 'E102', 'amountが負');
    }

    /**
     * 移動更新バリデーションエラーテスト（金額が0）
     */
    public function testMovePutAmountZero(): void
    {
        $this->assertPutErrorForAll(['amount' => 0], 400, 'E102', 'amountが0');
    }

    /**
     * 移動更新バリデーションエラーテスト（金額がない）
     */
    public function testMovePutAmountMissing(): void
    {
        $this->assertPutErrorUnsetForAll('amount', 400, 'E109', 'amountがない');
    }

    /**
     * 移動更新バリデーションエラーテスト（金額が null）
     */
    public function testMovePutAmountNull(): void
    {
        $this->assertPutErrorForAll(['amount' => null], 400, 'E101', 'amountがnull');
    }

    /**
     * 移動更新バリデーションエラーテスト（金額が文字）
     */
    public function testMovePutAmountString(): void
    {
        $this->assertPutErrorForAll(['amount' => ''], 400, 'E101', 'amountが空文字列');
        $this->assertPutErrorForAll(['amount' => 'aaa'], 400, 'E101', 'amountが文字列');
        $this->assertPutErrorForAll(['amount' => '1230'], 400, 'E101', 'amountが文字列数字');
    }

    /**
     * 移動更新バリデーションエラーテスト（項目が空）
     */
    public function testMovePutItemEmpty(): void
    {
        $this->assertPutErrorForAll(['item' => ''], 400, 'E105', 'itemが空文字列');
    }

    /**
     * 移動更新バリデーションエラーテスト（項目が null）
     */
    public function testMovePutItemNull(): void
    {
        $this->assertPutErrorForAll(['item' => null], 400, 'E101', 'itemがnull');
    }

    /**
     * 移動更新バリデーションエラーテスト（項目がない）
     */
    public function testMovePutItemMissing(): void
    {
        $this->assertPutErrorUnsetForAll('item', 400, 'E109', 'itemがない');
    }

    /**
     * 移動更新バリデーションエラーテスト（要素が移動ID）
     */
    public function testMovePutElementIsMoveId(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPutErrorForAll([$field => 1], 400, 'E108', "{$field}が移動ID");
        }
    }

    /**
     * 移動更新バリデーションエラーテスト（外部キー不正）
     */
    public function testMovePutForeignKeyInvalid(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPutErrorForAll([$field => 10000], 409, 'E302', "{$field}がない");
        }
    }

    /**
     * 移動更新バリデーションエラーテスト（要素パラメータなし）
     */
    public function testMovePutElementMissing(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPutErrorUnsetForAll($field, 400, 'E109', "{$field}が空");
        }
    }

    /**
     * 移動更新バリデーションエラーテスト（要素パラメータ null）
     */
    public function testMovePutElementNull(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPutErrorForAll([$field => null], 400, 'E101', "{$field}がnull");
        }
    }

    /**
     * 移動更新バリデーションエラーテスト（要素パラメータが文字列）
     */
    public function testMovePutElementString(): void
    {
        foreach ($this->elementIdFields() as $field) {
            $this->assertPutErrorForAll([$field => 'aaa'], 400, 'E101', "{$field}が文字列");
            $this->assertPutErrorForAll([$field => '12'], 400, 'E101', "{$field}が文字列数字");
        }
    }

    /**
     * 移動更新バリデーションエラーテスト（移動前後で同じ）
     */
    public function testMovePutSameBeforeAfter(): void
    {
        $this->assertPutErrorForAll(['before_id' => 2, 'after_id' => 2], 400, 'E107', '移動前後が同じ');
    }

    /**
     * 移動更新バリデーションエラーテスト（日付不正）
     */
    public function testMovePutDateInvalid(): void
    {
        $this->assertPutErrorUnsetForAll('date', 400, 'E109', 'dateがない');
        $this->assertPutErrorForAll(['date' => null], 400, 'E101', 'dateがnull');
        $this->assertPutErrorForAll(['date' => '2024-06-31'], 400, 'E103', '存在しない日付');
        $this->assertPutErrorForAll(['date' => '2025-02-29'], 400, 'E103', 'うるう年ではないうるう日');
    }

    /**
     * 移動更新テスト（属性名不正）
     */
    public function testMovePutInvalidAttributeName(): void
    {
        $response = $this->request->put('/moves/aaa/234', $this->validMove());
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E305', $response->jsonBody()['code'], '属性名が存在しない');
    }

    /**
     * 移動更新テスト（存在しない）
     */
    public function testMovePutNotFound(): void
    {
        foreach ($this->moveTypes() as $moveType) {
            $response = $this->request->put('/moves/' . $moveType . '/999999', $this->validMove());
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 更新対象のレコードがない");
        }
    }

    /**
     * 移動更新テスト（収支レコード）
     */
    public function testMovePutBalanceRecord(): void
    {
        $response = $this->request->get('/balances?limit=1');
        $id = $response->jsonBody()[0]['id'];

        foreach ($this->moveTypes() as $moveType) {
            $response = $this->request->put('/moves/' . $moveType . '/' . $id, $this->validMove());
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 収支レコード");
        }
    }

    /**
     * 移動更新テスト（前後のID）
     */
    public function testMovePutAdjacentIds(): void
    {
        foreach ($this->moveTypes() as $moveType) {
            $response = $this->request->post('/moves/' . $moveType, $this->validMove());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $response = $this->request->put('/moves/' . $moveType . '/' . ($id - 1), $this->validMove());
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 前のID");

            $response = $this->request->put('/moves/' . $moveType . '/' . ($id + 1), $this->validMove());
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 後のID");
        }
    }

    /**
     * 移動更新テスト（別属性の移動）
     */
    public function testMovePutDifferentAttribute(): void
    {
        // purposes で登録した移動を places で更新
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->put('/moves/places/' . $purposeId, $this->validMove());
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposesの移動をplacesで更新');

        // places で登録した移動を purposes で更新
        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        $response = $this->request->put('/moves/purposes/' . $placeId, $this->validMove());
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'placesの移動をpurposesで更新');
    }

    /**
     * 移動削除テスト
     */
    public function testMoveDelete(): void
    {
        foreach ($this->moveTypes() as $moveType) {
            $response = $this->request->post('/moves/' . $moveType, $this->validMove());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $response = $this->request->delete('/moves/' . $moveType . '/' . $id);
            Assert::assertStatusCode200($response->statusCode());

            $response = $this->request->get('/moves/' . $moveType . '/' . $id);
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 削除後の取得");
        }
    }

    /**
     * 移動削除テスト（存在しない）
     */
    public function testMoveDeleteNotFound(): void
    {
        foreach ($this->moveTypes() as $moveType) {
            $response = $this->request->delete('/moves/' . $moveType . '/999999');
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 削除対象のレコードがない");
        }
    }

    /**
     * 移動削除テスト（属性名不正）
     */
    public function testMoveDeleteInvalidAttributeName(): void
    {
        $response = $this->request->delete('/moves/aaa/232');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E305', $response->jsonBody()['code'], '属性名が存在しない');
    }

    /**
     * 移動削除テスト（収支レコード）
     */
    public function testMoveDeleteBalanceRecord(): void
    {
        foreach ($this->moveTypes() as $moveType) {
            $response = $this->request->delete('/moves/' . $moveType . '/10');
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 収支レコード");
        }
    }

    /**
     * 移動削除テスト（前後のID）
     */
    public function testMoveDeleteAdjacentIds(): void
    {
        foreach ($this->moveTypes() as $moveType) {
            $response = $this->request->post('/moves/' . $moveType, $this->validMove());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $response = $this->request->delete('/moves/' . $moveType . '/' . ($id - 1));
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 前のID");

            $response = $this->request->delete('/moves/' . $moveType . '/' . ($id + 1));
            Assert::assertStatusCode404($response->statusCode());
            Assert::assertSame('E301', $response->jsonBody()['code'], "{$moveType} 後のID");
        }
    }

    /**
     * 移動削除テスト（別属性の移動）
     */
    public function testMoveDeleteDifferentAttribute(): void
    {
        // purposes で登録した移動を places で削除
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->delete('/moves/places/' . $purposeId);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposesの移動をplacesで削除');

        // places で登録した移動を purposes で削除
        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        $response = $this->request->delete('/moves/purposes/' . $placeId);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'placesの移動をpurposesで削除');
    }

    /**
     * 移動登録バリデーションエラーテスト（グループID不正）
     */
    public function testMovePostGroupIdInvalid(): void
    {
        $this->assertPostErrorForAll(['group_id' => 0], 400, 'E102', 'group_idが0');
        $this->assertPostErrorForAll(['group_id' => ''], 400, 'E101', 'group_idが空文字列');
        $this->assertPostErrorForAll(['group_id' => 'aaa'], 400, 'E101', 'group_idが文字列');
        $this->assertPostErrorForAll(['group_id' => '5'], 400, 'E101', 'group_idが文字列数字');
    }

    /**
     * 移動更新バリデーションエラーテスト（グループID不正）
     */
    public function testMovePutGroupIdInvalid(): void
    {
        $this->assertPutErrorUnsetForAll('group_id', 400, 'E109', 'group_idがない');
        $this->assertPutErrorForAll(['group_id' => null], 400, 'E101', 'group_idがnull');
        $this->assertPutErrorForAll(['group_id' => 0], 400, 'E102', 'group_idが0');
        $this->assertPutErrorForAll(['group_id' => ''], 400, 'E101', 'group_idが文字列');
        $this->assertPutErrorForAll(['group_id' => '5'], 400, 'E101', 'group_idが文字列数字');
    }

    /**
     * 認証なし
     */
    public function testMoveWithoutAuth(): void
    {
        $noSessionRequest = new Request();

        foreach ($this->moveTypes() as $moveType) {
            $response = $noSessionRequest->get('/moves/' . $moveType);
            Assert::assertStatusCode401($response->statusCode());
            Assert::assertSame('E201', $response->jsonBody()['code'], "認証無し一覧取得 {$moveType}");

            $response = $noSessionRequest->get('/moves/' . $moveType . '/10');
            Assert::assertStatusCode401($response->statusCode());
            Assert::assertSame('E201', $response->jsonBody()['code'], "認証無し取得 {$moveType}");

            $response = $noSessionRequest->post('/moves/' . $moveType, $this->validMove());
            Assert::assertStatusCode401($response->statusCode());
            Assert::assertSame('E201', $response->jsonBody()['code'], "認証無し登録 {$moveType}");

            $response = $noSessionRequest->put('/moves/' . $moveType . '/10', $this->validMove());
            Assert::assertStatusCode401($response->statusCode());
            Assert::assertSame('E201', $response->jsonBody()['code'], "認証無し更新 {$moveType}");

            $response = $noSessionRequest->delete('/moves/' . $moveType . '/10');
            Assert::assertStatusCode401($response->statusCode());
            Assert::assertSame('E201', $response->jsonBody()['code'], "認証無し削除 {$moveType}");
        }
    }

    private function validMove(): array
    {
        return [
            'amount' => 500,
            'item' => 'テスト移動',
            'before_id' => 2,
            'after_id' => 3,
            'date' => '2024-10-23',
            'group_id' => 2,
        ];
    }

    private function moveTypes(): array
    {
        return ['purposes', 'places'];
    }

    private function elementIdFields(): array
    {
        return ['before_id', 'after_id'];
    }

    /**
     * 移動の各フィールドをアサートする
     */
    private function assertMoveFields(array $move, array $expected, string $prefix): void
    {
        Assert::assertSame($expected['amount'], $move['amount'], "{$prefix}の amount");
        Assert::assertSame($expected['item'], $move['item'], "{$prefix}の item");
        Assert::assertSame($expected['date'], $move['date'], "{$prefix}の date");
        Assert::assertSame($expected['before_id'], $move['before_id'], "{$prefix}の before_id");
        Assert::assertSame($expected['after_id'], $move['after_id'], "{$prefix}の after_id");
        Assert::assertSame($expected['group_id'] ?? null, $move['group_id'], "{$prefix}の group_id");
    }

    /**
     * 移動CRUDの共通テスト
     */
    private function assertMoveCRUD(string $moveType): void
    {
        $createData = $this->validMove();
        $updateData = [
            'amount' => 1000,
            'item' => 'テスト移動更新後',
            'before_id' => 4,
            'after_id' => 5,
            'date' => '2024-11-01',
            'group_id' => 5,
        ];

        // 登録
        $response = $this->request->post('/moves/' . $moveType, $createData);
        Assert::assertStatusCode200($response->statusCode());
        $this->assertMoveFields($response->jsonBody(), $createData, '移動登録後');

        $id = $response->jsonBody()['id'];

        // 個別取得
        $response = $this->request->get('/moves/' . $moveType . '/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $this->assertMoveFields($response->jsonBody(), $createData, '移動取得後');

        $move = $response->jsonBody();
        Assert::assertSame(substr($moveType, 0, -1) . '_ele_desc_2', $move['before_description'], "{$moveType} の before_description");
        Assert::assertSame(substr($moveType, 0, -1) . '_ele_desc_3', $move['after_description'], "{$moveType} の after_description");

        // 更新
        $response = $this->request->put('/moves/' . $moveType . '/' . $id, $updateData);
        Assert::assertStatusCode200($response->statusCode());
        $this->assertMoveFields($response->jsonBody(), $updateData, '更新後');

        // 削除
        $response = $this->request->delete('/moves/' . $moveType . '/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $this->assertMoveFields($response->jsonBody(), $updateData, '削除後');

        // 削除後に取得すると 404 になること
        $response = $this->request->get('/moves/' . $moveType . '/' . $id);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '削除後の取得');
    }

    /**
     * 全移動タイプで POST エラーをアサートする（フィールド上書き）
     */
    private function assertPostErrorForAll(array $overrides, int $statusCode, string $errorCode, string $message): void
    {
        foreach ($this->moveTypes() as $moveType) {
            $move = array_merge($this->validMove(), $overrides);
            $response = $this->request->post('/moves/' . $moveType, $move);
            $this->assertErrorResponse($response, $statusCode, $errorCode, "{$moveType} {$message}");
        }
    }

    /**
     * 全移動タイプで POST エラーをアサートする（フィールド削除）
     */
    private function assertPostErrorUnsetForAll(string $field, int $statusCode, string $errorCode, string $message): void
    {
        foreach ($this->moveTypes() as $moveType) {
            $move = $this->validMove();
            unset($move[$field]);
            $response = $this->request->post('/moves/' . $moveType, $move);
            $this->assertErrorResponse($response, $statusCode, $errorCode, "{$moveType} {$message}");
        }
    }

    /**
     * 全移動タイプで PUT エラーをアサートする（フィールド上書き）
     */
    private function assertPutErrorForAll(array $overrides, int $statusCode, string $errorCode, string $message): void
    {
        $ids = $this->createMoveRecords();

        foreach ($this->moveTypes() as $moveType) {
            $move = array_merge($this->validMove(), $overrides);
            $response = $this->request->put('/moves/' . $moveType . '/' . $ids[$moveType], $move);
            $this->assertErrorResponse($response, $statusCode, $errorCode, "{$moveType} {$message}");
        }
    }

    /**
     * 全移動タイプで PUT エラーをアサートする（フィールド削除）
     */
    private function assertPutErrorUnsetForAll(string $field, int $statusCode, string $errorCode, string $message): void
    {
        $ids = $this->createMoveRecords();

        foreach ($this->moveTypes() as $moveType) {
            $move = $this->validMove();
            unset($move[$field]);
            $response = $this->request->put('/moves/' . $moveType . '/' . $ids[$moveType], $move);
            $this->assertErrorResponse($response, $statusCode, $errorCode, "{$moveType} {$message}");
        }
    }

    /**
     * 各移動タイプのレコードを作成してIDを返す
     */
    private function createMoveRecords(): array
    {
        $ids = [];
        foreach ($this->moveTypes() as $moveType) {
            $response = $this->request->post('/moves/' . $moveType, $this->validMove());
            Assert::assertStatusCode200($response->statusCode());
            $ids[$moveType] = $response->jsonBody()['id'];
        }
        return $ids;
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
