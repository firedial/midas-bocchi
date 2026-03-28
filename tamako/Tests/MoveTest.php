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

        $move = $response->jsonBody()[0];
        Assert::assertSame(500, $move['amount'], '一覧取得の amount');
        Assert::assertSame('テスト移動', $move['item'], '一覧取得の item');
        Assert::assertSame('2024-10-23', $move['date'], '一覧取得の date');
        Assert::assertSame(2, $move['before_id'], '一覧取得の before_id');
        Assert::assertSame(3, $move['after_id'], '一覧取得の after_id');
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

        $move = $response->jsonBody()[0];
        Assert::assertSame(500, $move['amount'], '一覧取得の amount');
        Assert::assertSame('テスト移動', $move['item'], '一覧取得の item');
        Assert::assertSame('2024-10-23', $move['date'], '一覧取得の date');
        Assert::assertSame(2, $move['before_id'], '一覧取得の before_id');
        Assert::assertSame(3, $move['after_id'], '一覧取得の after_id');
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
        // 登録
        $response = $this->request->post('/moves/purposes', [
            'amount' => 500,
            'item' => 'テスト移動',
            'before_id' => 2,
            'after_id' => 3,
            'date' => '2024-10-23',
        ]);
        Assert::assertStatusCode200($response->statusCode());
        $move = $response->jsonBody();
        Assert::assertSame(500, $move['amount'], '移動登録後の amount');
        Assert::assertSame('テスト移動', $move['item'], '移動登録後の item');
        Assert::assertSame('2024-10-23', $move['date'], '移動登録後の date');
        Assert::assertSame(2, $move['before_id'], '移動登録後の before_id');
        Assert::assertSame(3, $move['after_id'], '移動登録後の after_id');

        $id = $move['id'];

        // 個別取得
        $response = $this->request->get('/moves/purposes/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $move = $response->jsonBody();
        Assert::assertSame(500, $move['amount'], '移動取得後の amount');
        Assert::assertSame('テスト移動', $move['item'], '移動取得後の item');
        Assert::assertSame('2024-10-23', $move['date'], '移動取得後の date');
        Assert::assertSame(2, $move['before_id'], '移動取得後の before_id');
        Assert::assertSame(3, $move['after_id'], '移動取得後の after_id');

        // 更新
        $response = $this->request->put('/moves/purposes/' . $id, [
            'amount' => 1000,
            'item' => 'テスト移動更新後',
            'before_id' => 4,
            'after_id' => 5,
            'date' => '2024-11-01',
        ]);
        Assert::assertStatusCode200($response->statusCode());
        $move = $response->jsonBody();
        Assert::assertSame(1000, $move['amount'], '更新後の amount');
        Assert::assertSame('テスト移動更新後', $move['item'], '更新後の item');
        Assert::assertSame('2024-11-01', $move['date'], '更新後の date');
        Assert::assertSame(4, $move['before_id'], '更新後の before_id');
        Assert::assertSame(5, $move['after_id'], '更新後の after_id');

        // 削除
        $response = $this->request->delete('/moves/purposes/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $move = $response->jsonBody();
        Assert::assertSame(1000, $move['amount'], '削除後の amount');
        Assert::assertSame('テスト移動更新後', $move['item'], '削除後の item');
        Assert::assertSame('2024-11-01', $move['date'], '削除後の date');
        Assert::assertSame(4, $move['before_id'], '削除後の before_id');
        Assert::assertSame(5, $move['after_id'], '削除後の after_id');

        // 削除後に取得すると 404 になること
        $response = $this->request->get('/moves/purposes/' . $id);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '削除後の取得');
    }

    /**
     * 移動CRUDテスト - places
     */
    public function testMoveCRUDPlaces(): void
    {
        // 登録
        $response = $this->request->post('/moves/places', [
            'amount' => 500,
            'item' => 'テスト移動',
            'before_id' => 2,
            'after_id' => 3,
            'date' => '2024-10-23',
        ]);
        Assert::assertStatusCode200($response->statusCode());
        $move = $response->jsonBody();
        Assert::assertSame(500, $move['amount'], '移動登録後の amount');
        Assert::assertSame('テスト移動', $move['item'], '移動登録後の item');
        Assert::assertSame('2024-10-23', $move['date'], '移動登録後の date');
        Assert::assertSame(2, $move['before_id'], '移動登録後の before_id');
        Assert::assertSame(3, $move['after_id'], '移動登録後の after_id');

        $id = $move['id'];

        // 個別取得
        $response = $this->request->get('/moves/places/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $move = $response->jsonBody();
        Assert::assertSame(500, $move['amount'], '移動取得後の amount');
        Assert::assertSame('テスト移動', $move['item'], '移動取得後の item');
        Assert::assertSame('2024-10-23', $move['date'], '移動取得後の date');
        Assert::assertSame(2, $move['before_id'], '移動取得後の before_id');
        Assert::assertSame(3, $move['after_id'], '移動取得後の after_id');

        // 更新
        $response = $this->request->put('/moves/places/' . $id, [
            'amount' => 1000,
            'item' => 'テスト移動更新後',
            'before_id' => 4,
            'after_id' => 5,
            'date' => '2024-11-01',
        ]);
        Assert::assertStatusCode200($response->statusCode());
        $move = $response->jsonBody();
        Assert::assertSame(1000, $move['amount'], '更新後の amount');
        Assert::assertSame('テスト移動更新後', $move['item'], '更新後の item');
        Assert::assertSame('2024-11-01', $move['date'], '更新後の date');
        Assert::assertSame(4, $move['before_id'], '更新後の before_id');
        Assert::assertSame(5, $move['after_id'], '更新後の after_id');

        // 削除
        $response = $this->request->delete('/moves/places/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $move = $response->jsonBody();
        Assert::assertSame(1000, $move['amount'], '削除後の amount');
        Assert::assertSame('テスト移動更新後', $move['item'], '削除後の item');
        Assert::assertSame('2024-11-01', $move['date'], '削除後の date');
        Assert::assertSame(4, $move['before_id'], '削除後の before_id');
        Assert::assertSame(5, $move['after_id'], '削除後の after_id');

        // 削除後に取得すると 404 になること
        $response = $this->request->get('/moves/places/' . $id);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '削除後の取得');
    }

    /**
     * うるう年テスト
     */
    public function testMoveLeapDay(): void
    {
        // 正常系

        // うるう年 - purposes
        $response = $this->request->post('/moves/purposes', [
            'amount' => 500,
            'item' => 'テスト移動',
            'before_id' => 2,
            'after_id' => 3,
            'date' => '2024-02-29',
        ]);
        Assert::assertStatusCode200($response->statusCode());
        $move = $response->jsonBody();
        Assert::assertSame('2024-02-29', $move['date'], '閏日の登録 purposes');

        $purposeId = $move['id'];

        // うるう年 - places
        $response = $this->request->post('/moves/places', [
            'amount' => 500,
            'item' => 'テスト移動',
            'before_id' => 2,
            'after_id' => 3,
            'date' => '2024-02-29',
        ]);
        Assert::assertStatusCode200($response->statusCode());
        $move = $response->jsonBody();
        Assert::assertSame('2024-02-29', $move['date'], '閏日の登録 places');

        $placeId = $move['id'];

        // 更新 - purposes
        $response = $this->request->put('/moves/purposes/' . $purposeId, [
            'amount' => 1000,
            'item' => 'テスト移動更新後',
            'before_id' => 2,
            'after_id' => 3,
            'date' => '2020-02-29',
        ]);
        Assert::assertStatusCode200($response->statusCode());
        $move = $response->jsonBody();
        Assert::assertSame('2020-02-29', $move['date'], '閏日の更新 purposes');

        // 更新 - places
        $response = $this->request->put('/moves/places/' . $placeId, [
            'amount' => 1000,
            'item' => 'テスト移動更新後',
            'before_id' => 2,
            'after_id' => 3,
            'date' => '2020-02-29',
        ]);
        Assert::assertStatusCode200($response->statusCode());
        $move = $response->jsonBody();
        Assert::assertSame('2020-02-29', $move['date'], '閏日の更新 places');

        // 異常系

        // うるう年でない年の2/29 - purposes
        $response = $this->request->post('/moves/purposes', [
            'amount' => 500,
            'item' => 'テスト移動',
            'before_id' => 2,
            'after_id' => 3,
            'date' => '2023-02-29',
        ]);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'うるう年ではない年のうるう日登録 purposes');

        // うるう年でない年の2/29 - places
        $response = $this->request->post('/moves/places', [
            'amount' => 500,
            'item' => 'テスト移動',
            'before_id' => 2,
            'after_id' => 3,
            'date' => '2023-02-29',
        ]);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'うるう年ではない年のうるう日登録 places');

        // 更新 - purposes
        $response = $this->request->put('/moves/purposes/' . $purposeId, [
            'amount' => 1000,
            'item' => 'テスト移動更新後',
            'before_id' => 2,
            'after_id' => 3,
            'date' => '2022-02-29',
        ]);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'うるう年ではない年のうるう日更新 purposes');

        // 更新 - places
        $response = $this->request->put('/moves/places/' . $placeId, [
            'amount' => 1000,
            'item' => 'テスト移動更新後',
            'before_id' => 2,
            'after_id' => 3,
            'date' => '2022-02-29',
        ]);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'うるう年ではない年のうるう日更新 places');
    }

    /**
     * 存在しない移動テスト
     */
    public function testMoveShowNotFound(): void
    {
        // purposes
        $response = $this->request->get('/moves/purposes/999999');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 取得');

        $response = $this->request->put('/moves/purposes/999999', $this->validMove());
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 更新');

        $response = $this->request->delete('/moves/purposes/999999');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 削除');

        // places
        $response = $this->request->get('/moves/places/999999');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 取得');

        $response = $this->request->put('/moves/places/999999', $this->validMove());
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 更新');

        $response = $this->request->delete('/moves/places/999999');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 削除');
    }

    /**
     * 移動登録バリデーションエラーテスト（金額が負）
     */
    public function testMovePostAmountNegative(): void
    {
        $move = $this->validMove();
        $move['amount'] = -500;

        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E102', $response->jsonBody()['code'], 'purposes amountが負');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E102', $response->jsonBody()['code'], 'places amountが負');
    }

    /**
     * 移動登録バリデーションエラーテスト（金額が0）
     */
    public function testMovePostAmountZero(): void
    {
        $move = $this->validMove();
        $move['amount'] = 0;

        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E102', $response->jsonBody()['code'], 'purposes amountが0');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E102', $response->jsonBody()['code'], 'places amountが0');
    }

    /**
     * 移動登録バリデーションエラーテスト（金額がない）
     */
    public function testMovePostAmountMissing(): void
    {
        $move = $this->validMove();
        unset($move['amount']);

        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'purposes amountがない');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'places amountがない');
    }

    /**
     * 移動登録バリデーションエラーテスト（金額が文字）
     */
    public function testMovePostAmountString(): void
    {
        $move = $this->validMove();
        $move['amount'] = 'aaa';

        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'purposes amountが文字列');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'places amountが文字列');

        $move = $this->validMove();
        $move['amount'] = '100';

        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'purposes amountが文字列');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'places amountが文字列');
    }

    /**
     * 移動登録バリデーションエラーテスト（項目が空）
     */
    public function testMovePostItemEmpty(): void
    {
        $move = $this->validMove();
        $move['item'] = '';

        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'purposes itemが空');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'places itemが空');
    }

    /**
     * 移動登録バリデーションエラーテスト（項目がない）
     */
    public function testMovePostItemMissing(): void
    {
        $move = $this->validMove();
        unset($move['item']);

        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'purposes itemがない');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'places itemがない');
    }

    /**
     * 移動登録バリデーションエラーテスト（項目の長さ）
     */
    public function testMovePostItemLength(): void
    {
        // 50文字
        $move = $this->validMove();
        $move['item'] = 'あいうえおかきくけこさしすせそたちつてとなにぬねのあいうえおかきくけこさしすせそたちつてとなにぬねの';
        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode200($response->statusCode());

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode200($response->statusCode());

        // 51文字
        $move = $this->validMove();
        $move['item'] = 'あいうえおかきくけこさしすせそたちつてとなにぬねのあいうえおかきくけこさしすせそたちつてとなにぬねのあ';
        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E105', $response->jsonBody()['code'], 'purposes item長い');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E105', $response->jsonBody()['code'], 'places item長い');
    }

    /**
     * 移動登録バリデーションエラーテスト（要素が移動ID）
     */
    public function testMovePostElementIsMoveId(): void
    {
        // before_id が移動ID
        $move = $this->validMove();
        $move['before_id'] = 1;
        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E108', $response->jsonBody()['code'], 'purposes before_idが移動ID');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E108', $response->jsonBody()['code'], 'places before_idが移動ID');

        // after_id が移動ID
        $move = $this->validMove();
        $move['after_id'] = 1;
        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E108', $response->jsonBody()['code'], 'purposes after_idが移動ID');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E108', $response->jsonBody()['code'], 'places after_idが移動ID');
    }

    /**
     * 移動登録バリデーションエラーテスト（外部キー不正）
     */
    public function testMovePostForeignKeyInvalid(): void
    {
        // before_id が存在しない
        $move = $this->validMove();
        $move['before_id'] = 10000;
        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode409($response->statusCode());
        Assert::assertSame('E302', $response->jsonBody()['code'], 'purposes before_idがない');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode409($response->statusCode());
        Assert::assertSame('E302', $response->jsonBody()['code'], 'places before_idがない');

        // after_id が存在しない
        $move = $this->validMove();
        $move['after_id'] = 10000;
        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode409($response->statusCode());
        Assert::assertSame('E302', $response->jsonBody()['code'], 'purposes after_idがない');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode409($response->statusCode());
        Assert::assertSame('E302', $response->jsonBody()['code'], 'places after_idがない');
    }

    /**
     * 移動登録バリデーションエラーテスト（要素パラメータなし）
     */
    public function testMovePostElementMissing(): void
    {
        // before_id なし
        $move = $this->validMove();
        unset($move['before_id']);
        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'purposes before_idが空');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'places before_idが空');

        // after_id なし
        $move = $this->validMove();
        unset($move['after_id']);
        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'purposes after_idが空');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'places after_idが空');
    }

    /**
     * 移動登録バリデーションエラーテスト（要素パラメータが文字列）
     */
    public function testMovePostElementString(): void
    {
        // before_id が文字列
        $move = $this->validMove();
        $move['before_id'] = 'aaa';
        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'purposes before_idが文字列');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'places before_idが文字列');

        // after_id が文字列
        $move = $this->validMove();
        $move['after_id'] = 'aaa';
        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'purposes after_idが文字列');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'places after_idが文字列');

        // before_id が文字列数字
        $move = $this->validMove();
        $move['before_id'] = '5';
        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'purposes before_idが文字列');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'places before_idが文字列');

        // after_id が文字列
        $move = $this->validMove();
        $move['after_id'] = '5';
        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'purposes after_idが文字列');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'places after_idが文字列');
    }

    /**
     * 移動登録バリデーションエラーテスト（移動前後で同じ）
     */
    public function testMovePostSameBeforeAfter(): void
    {
        $move = $this->validMove();
        $move['before_id'] = 2;
        $move['after_id'] = 2;

        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E107', $response->jsonBody()['code'], 'purposes 移動前後が同じ');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E107', $response->jsonBody()['code'], 'places 移動前後が同じ');
    }

    /**
     * 移動登録バリデーションエラーテスト（日付不正）
     */
    public function testMovePostDateInvalid(): void
    {
        $move = $this->validMove();
        $move['date'] = 'invalid-date';

        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'purposes 日付が不正');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'places 日付が不正');
    }

    /**
     * 移動登録バリデーションエラーテスト（日付がない）
     */
    public function testMovePostDateMissing(): void
    {
        $move = $this->validMove();
        unset($move['date']);

        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'purposes 日付がない');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'places 日付がない');
    }

    /**
     * 移動登録バリデーションエラーテスト（存在しない日付）
     */
    public function testMovePostDateNotExist(): void
    {
        $move = $this->validMove();
        $move['date'] = '2024-06-31';

        $response = $this->request->post('/moves/purposes', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'purposes 存在しない日付');

        $response = $this->request->post('/moves/places', $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'places 存在しない日付');
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

        // 収支レコードのIDで移動を取得しようとすると 404
        $response = $this->request->get('/moves/purposes/' . $id);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 収支レコード');

        $response = $this->request->get('/moves/places/' . $id);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 収支レコード');
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
        // purposes で登録
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        // 前後のID
        $response = $this->request->get('/moves/purposes/' . ($purposeId - 1));
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 前のID');

        $response = $this->request->get('/moves/purposes/' . ($purposeId + 1));
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 後のID');

        // places で登録
        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        // 前後のID
        $response = $this->request->get('/moves/places/' . ($placeId - 1));
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 前のID');

        $response = $this->request->get('/moves/places/' . ($placeId + 1));
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 後のID');
    }

    /**
     * 移動更新バリデーションエラーテスト（金額が負）
     */
    public function testMovePutAmountNegative(): void
    {
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        $move = $this->validMove();
        $move['amount'] = -500;

        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E102', $response->jsonBody()['code'], 'purposes amountが負');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E102', $response->jsonBody()['code'], 'places amountが負');
    }

    /**
     * 移動更新バリデーションエラーテスト（金額が0）
     */
    public function testMovePutAmountZero(): void
    {
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        $move = $this->validMove();
        $move['amount'] = 0;

        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E102', $response->jsonBody()['code'], 'purposes amountが0');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E102', $response->jsonBody()['code'], 'places amountが0');
    }

    /**
     * 移動更新バリデーションエラーテスト（金額がない）
     */
    public function testMovePutAmountMissing(): void
    {
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        $move = $this->validMove();
        unset($move['amount']);

        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'purposes amountがない');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'places amountがない');
    }

    /**
     * 移動更新バリデーションエラーテスト（金額が文字）
     */
    public function testMovePutAmountString(): void
    {
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        $move = $this->validMove();
        $move['amount'] = 'aaa';

        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'purposes amountが文字列');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'places amountが文字列');

        $move = $this->validMove();
        $move['amount'] = '1230';

        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'purposes amountが文字列');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'places amountが文字列');
    }

    /**
     * 移動更新バリデーションエラーテスト（項目が空）
     */
    public function testMovePutItemEmpty(): void
    {
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        $move = $this->validMove();
        $move['item'] = '';

        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'purposes itemが空文字列');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'places itemが空文字列');
    }

    /**
     * 移動更新バリデーションエラーテスト（項目がない）
     */
    public function testMovePutItemMissing(): void
    {
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        $move = $this->validMove();
        unset($move['item']);

        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'purposes itemがない');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'places itemがない');
    }

    /**
     * 移動更新バリデーションエラーテスト（要素が移動ID）
     */
    public function testMovePutElementIsMoveId(): void
    {
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        // before_id が移動ID
        $move = $this->validMove();
        $move['before_id'] = 1;
        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E108', $response->jsonBody()['code'], 'purposes before_idが移動ID');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E108', $response->jsonBody()['code'], 'places before_idが移動ID');

        // after_id が移動ID
        $move = $this->validMove();
        $move['after_id'] = 1;
        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E108', $response->jsonBody()['code'], 'purposes after_idが移動ID');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E108', $response->jsonBody()['code'], 'places after_idが移動ID');
    }

    /**
     * 移動更新バリデーションエラーテスト（外部キー不正）
     */
    public function testMovePutForeignKeyInvalid(): void
    {
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        // before_id が存在しない
        $move = $this->validMove();
        $move['before_id'] = 10000;
        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode409($response->statusCode());
        Assert::assertSame('E302', $response->jsonBody()['code'], 'purposes before_idがない');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode409($response->statusCode());
        Assert::assertSame('E302', $response->jsonBody()['code'], 'places before_idがない');

        // after_id が存在しない
        $move = $this->validMove();
        $move['after_id'] = 10000;
        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode409($response->statusCode());
        Assert::assertSame('E302', $response->jsonBody()['code'], 'purposes after_idがない');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode409($response->statusCode());
        Assert::assertSame('E302', $response->jsonBody()['code'], 'places after_idがない');
    }

    /**
     * 移動更新バリデーションエラーテスト（要素パラメータなし）
     */
    public function testMovePutElementMissing(): void
    {
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        // before_id なし
        $move = $this->validMove();
        unset($move['before_id']);
        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'purposes before_idが空');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'places before_idが空');

        // after_id なし
        $move = $this->validMove();
        unset($move['after_id']);
        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'purposes after_idが空');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'places after_idが空');
    }

    /**
     * 移動更新バリデーションエラーテスト（要素パラメータが文字列）
     */
    public function testMovePutElementString(): void
    {
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        // before_id が文字列
        $move = $this->validMove();
        $move['before_id'] = 'aaa';
        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'purposes before_idが文字列');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'places before_idが文字列');

        // after_id が文字列
        $move = $this->validMove();
        $move['after_id'] = 'aaa';
        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'purposes after_idが文字列');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'places after_idが文字列');

        // before_id が文字列
        $move = $this->validMove();
        $move['before_id'] = '12';
        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'purposes before_idが文字列');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'places before_idが文字列');

        // after_id が文字列
        $move = $this->validMove();
        $move['after_id'] = '12';
        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'purposes after_idが文字列');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E101', $response->jsonBody()['code'], 'places after_idが文字列');
    }

    /**
     * 移動更新バリデーションエラーテスト（移動前後で同じ）
     */
    public function testMovePutSameBeforeAfter(): void
    {
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        $move = $this->validMove();
        $move['before_id'] = 2;
        $move['after_id'] = 2;

        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E107', $response->jsonBody()['code'], 'purposes 移動前後が同じ');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E107', $response->jsonBody()['code'], 'places 移動前後が同じ');
    }

    /**
     * 移動更新バリデーションエラーテスト（日付不正）
     */
    public function testMovePutDateInvalid(): void
    {
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        // 日付なし
        $move = $this->validMove();
        unset($move['date']);
        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'purposes dateがない');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E109', $response->jsonBody()['code'], 'places dateがない');

        // 存在しない日付
        $move = $this->validMove();
        $move['date'] = '2024-06-31';
        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'purposes 存在しない日付');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'places 存在しない日付');

        // うるう年でない年の2/29
        $move = $this->validMove();
        $move['date'] = '2025-02-29';
        $response = $this->request->put('/moves/purposes/' . $purposeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'purposes うるう年ではないうるう日');

        $response = $this->request->put('/moves/places/' . $placeId, $move);
        Assert::assertStatusCode400($response->statusCode());
        Assert::assertSame('E103', $response->jsonBody()['code'], 'places うるう年ではないうるう日');
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
        $move = $this->validMove();

        // purposes
        $response = $this->request->put('/moves/purposes/999999', $move);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 更新対象のレコードがない');

        // places
        $response = $this->request->put('/moves/places/999999', $move);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 更新対象のレコードがない');
    }

    /**
     * 移動更新テスト（収支レコード）
     */
    public function testMovePutBalanceRecord(): void
    {
        $response = $this->request->get('/balances?limit=1');
        $id = $response->jsonBody()[0]['id'];

        $move = $this->validMove();
        $response = $this->request->put('/moves/purposes/' . $id, $move);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 収支レコード');

        $response = $this->request->put('/moves/places/' . $id, $move);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 収支レコード');
    }

    /**
     * 移動更新テスト（前後のID）
     */
    public function testMovePutAdjacentIds(): void
    {
        // purposes で登録
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $move = $this->validMove();
        $response = $this->request->put('/moves/purposes/' . ($purposeId - 1), $move);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 前のID');

        $response = $this->request->put('/moves/purposes/' . ($purposeId + 1), $move);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 後のID');

        // places で登録
        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        $response = $this->request->put('/moves/places/' . ($placeId - 1), $move);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 前のID');

        $response = $this->request->put('/moves/places/' . ($placeId + 1), $move);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 後のID');
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

        $move = $this->validMove();
        $response = $this->request->put('/moves/places/' . $purposeId, $move);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposesの移動をplacesで更新');

        // places で登録した移動を purposes で更新
        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        $response = $this->request->put('/moves/purposes/' . $placeId, $move);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'placesの移動をpurposesで更新');
    }

    /**
     * 移動削除テスト
     */
    public function testMoveDelete(): void
    {
        // purposes
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->delete('/moves/purposes/' . $purposeId);
        Assert::assertStatusCode200($response->statusCode());

        $response = $this->request->get('/moves/purposes/' . $purposeId);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 削除後の取得');

        // places
        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        $response = $this->request->delete('/moves/places/' . $placeId);
        Assert::assertStatusCode200($response->statusCode());

        $response = $this->request->get('/moves/places/' . $placeId);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 削除後の取得');
    }

    /**
     * 移動削除テスト（存在しない）
     */
    public function testMoveDeleteNotFound(): void
    {
        // purposes
        $response = $this->request->delete('/moves/purposes/999999');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 削除対象のレコードがない');

        // places
        $response = $this->request->delete('/moves/places/999999');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 削除対象のレコードがない');
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
        $response = $this->request->delete('/moves/purposes/10');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 収支レコード');

        $response = $this->request->delete('/moves/places/10');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 収支レコード');
    }

    /**
     * 移動削除テスト（前後のID）
     */
    public function testMoveDeleteAdjacentIds(): void
    {
        // purposes で登録
        $response = $this->request->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $purposeId = $response->jsonBody()['id'];

        $response = $this->request->delete('/moves/purposes/' . ($purposeId - 1));
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 前のID');

        $response = $this->request->delete('/moves/purposes/' . ($purposeId + 1));
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'purposes 後のID');

        // places で登録
        $response = $this->request->post('/moves/places', $this->validMove());
        Assert::assertStatusCode200($response->statusCode());
        $placeId = $response->jsonBody()['id'];

        $response = $this->request->delete('/moves/places/' . ($placeId - 1));
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 前のID');

        $response = $this->request->delete('/moves/places/' . ($placeId + 1));
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], 'places 後のID');
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
     * 認証なし
     */
    public function testMoveWithoutAuth(): void
    {
        $noSessionRequest = new Request();

        // purposes
        $response = $noSessionRequest->get('/moves/purposes');
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し一覧取得 purposes');

        $response = $noSessionRequest->get('/moves/purposes/10');
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し取得 purposes');

        $response = $noSessionRequest->post('/moves/purposes', $this->validMove());
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し登録 purposes');

        $response = $noSessionRequest->put('/moves/purposes/10', $this->validMove());
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し更新 purposes');

        $response = $noSessionRequest->delete('/moves/purposes/10');
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し削除 purposes');

        // places
        $response = $noSessionRequest->get('/moves/places');
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し一覧取得 places');

        $response = $noSessionRequest->get('/moves/places/10');
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し取得 places');

        $response = $noSessionRequest->post('/moves/places', $this->validMove());
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し登録 places');

        $response = $noSessionRequest->put('/moves/places/10', $this->validMove());
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し更新 places');

        $response = $noSessionRequest->delete('/moves/places/10');
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し削除 places');
    }

    private function validMove(): array
    {
        return [
            'amount' => 500,
            'item' => 'テスト移動',
            'before_id' => 2,
            'after_id' => 3,
            'date' => '2024-10-23',
        ];
    }
}
