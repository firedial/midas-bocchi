<?php

require_once __DIR__ . '/../TestRunner/TestCase.php';

class TemplateTest extends TestCase
{
    /**
     * テンプレートCRUDテスト
     */
    public function testTemplateCRUD(): void
    {
        $createData = $this->validTemplate();
        $updateData = [
            'name' => 'テンプレート更新後',
            'details' => [
                [
                    'type' => 1,
                    'amount' => -200,
                    'item' => '更新収支',
                    'kind_element_id' => 12,
                    'purpose_element_id' => 13,
                    'place_element_id' => 14,
                ],
            ],
        ];

        // 登録
        $response = $this->request->post('/templates', $createData);
        Assert::assertStatusCode200($response->statusCode());
        Assert::assertSame($createData['name'], $response->jsonBody()['name'], '登録後のname');

        $id = $response->jsonBody()['id'];

        // 個別取得
        $response = $this->request->get('/templates/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        Assert::assertSame($createData['name'], $response->jsonBody()['name'], '取得後のname');
        Assert::assertSame(2, count($response->jsonBody()['details']), '取得後の明細件数');
        $this->assertDetailFields($response->jsonBody()['details'][0], $createData['details'][0], '取得後の明細1');
        $this->assertDetailFields($response->jsonBody()['details'][1], $createData['details'][1], '取得後の明細2');

        // 更新
        $response = $this->request->put('/templates/' . $id, $updateData);
        Assert::assertStatusCode200($response->statusCode());
        Assert::assertSame($updateData['name'], $response->jsonBody()['name'], '更新後のname');

        // 更新後の個別取得
        $response = $this->request->get('/templates/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        Assert::assertSame($updateData['name'], $response->jsonBody()['name'], '更新後の取得name');
        Assert::assertSame(1, count($response->jsonBody()['details']), '更新後の明細件数');

        // 削除
        $response = $this->request->delete('/templates/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        Assert::assertSame($updateData['name'], $response->jsonBody()['name'], '削除後のname');

        // 削除後に取得すると404
        $response = $this->request->get('/templates/' . $id);
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '削除後の取得');
    }

    /**
     * テンプレート一覧取得テスト
     */
    public function testTemplateGet(): void
    {
        $response = $this->request->get('/templates');
        Assert::assertStatusCode200($response->statusCode());
        $beforeCount = count($response->jsonBody());

        $response = $this->request->post('/templates', $this->validTemplate());
        Assert::assertStatusCode200($response->statusCode());

        $response = $this->request->get('/templates');
        Assert::assertStatusCode200($response->statusCode());
        Assert::assertSame($beforeCount + 1, count($response->jsonBody()), '登録後の件数');
    }

    /**
     * テンプレート一覧レスポンスボディテスト（明細を含まない）
     */
    public function testTemplateGetResponseBody(): void
    {
        $response = $this->request->post('/templates', $this->validTemplate());
        Assert::assertStatusCode200($response->statusCode());
        $id = $response->jsonBody()['id'];

        $response = $this->request->get('/templates');
        Assert::assertStatusCode200($response->statusCode());

        $found = null;
        foreach ($response->jsonBody() as $template) {
            if ($template['id'] === $id) {
                $found = $template;
                break;
            }
        }

        Assert::assertSame(true, !is_null($found), '一覧に登録したテンプレートが存在する');
        Assert::assertSame(false, isset($found['details']), '一覧にdetailsは含まれない');
    }

    /**
     * テンプレート詳細レスポンスボディテスト（seq昇順）
     */
    public function testTemplateShowDetails(): void
    {
        $response = $this->request->post('/templates', $this->validTemplate());
        Assert::assertStatusCode200($response->statusCode());
        $id = $response->jsonBody()['id'];

        $response = $this->request->get('/templates/' . $id);
        Assert::assertStatusCode200($response->statusCode());

        $details = $response->jsonBody()['details'];
        Assert::assertSame(2, count($details), '明細件数');
        Assert::assertSame(1, $details[0]['seq'], '明細1のseq');
        Assert::assertSame(2, $details[1]['seq'], '明細2のseq');
        $this->assertDetailFields($details[0], $this->validTemplate()['details'][0], '明細1');
        $this->assertDetailFields($details[1], $this->validTemplate()['details'][1], '明細2');
    }

    /**
     * 存在しないテンプレートのテスト
     */
    public function testTemplateNotFound(): void
    {
        $response = $this->request->get('/templates/999999');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '取得');

        $response = $this->request->put('/templates/999999', $this->validTemplate());
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '更新');

        $response = $this->request->delete('/templates/999999');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E301', $response->jsonBody()['code'], '削除');
    }

    /**
     * 認証なしテスト
     */
    public function testTemplateWithoutAuth(): void
    {
        $noSessionRequest = new Request();

        $response = $noSessionRequest->get('/templates');
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証なし一覧取得');

        $response = $noSessionRequest->get('/templates/1');
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証なし取得');

        $response = $noSessionRequest->post('/templates', $this->validTemplate());
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証なし登録');

        $response = $noSessionRequest->put('/templates/1', $this->validTemplate());
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証なし更新');

        $response = $noSessionRequest->delete('/templates/1');
        Assert::assertStatusCode401($response->statusCode());
        Assert::assertSame('E201', $response->jsonBody()['code'], '認証なし削除');
    }

    /**
     * テンプレート名バリデーションエラーテスト
     */
    public function testTemplateNameInvalid(): void
    {
        // name がない
        $this->assertPostErrorUnset('name', 400, 'E109', 'nameがない');

        // name が null
        $this->assertPostError(['name' => null], 400, 'E109', 'nameがnull');

        // name が21文字 (最大20文字)
        $this->assertPostError(['name' => 'あいうえおかきくけこさしすせそたちつてとな'], 400, 'E105', 'nameが21文字');

        // name が20文字 (正常系)
        $template = $this->validTemplate();
        $template['name'] = 'あいうえおかきくけこさしすせそたちつてと';
        $response = $this->request->post('/templates', $template);
        Assert::assertStatusCode200($response->statusCode());
    }

    /**
     * details バリデーションエラーテスト
     */
    public function testTemplateDetailsInvalid(): void
    {
        // details がない
        $this->assertPostErrorUnset('details', 400, 'E109', 'detailsがない');

        // details が null
        $this->assertPostError(['details' => null], 400, 'E109', 'detailsがnull');

        // details が空配列
        $this->assertPostError(['details' => []], 400, 'E109', 'detailsが空配列');
    }

    /**
     * 明細 type バリデーションエラーテスト
     */
    public function testTemplateDetailTypeInvalid(): void
    {
        // type がない
        $this->assertPostDetail1ErrorUnset('type', 400, 'E109', 'typeがない');

        // type が null
        $this->assertPostDetail1Error(['type' => null], 400, 'E109', 'typeがnull');

        // type が文字列
        $this->assertPostDetail1Error(['type' => 'aaa'], 400, 'E101', 'typeが文字列');

        // type が文字列数字
        $this->assertPostDetail1Error(['type' => '1'], 400, 'E101', 'typeが文字列数字');

        // type が範囲外 (0)
        $this->assertPostDetail1Error(['type' => 0], 400, 'E106', 'typeが0');

        // type が範囲外 (4)
        $this->assertPostDetail1Error(['type' => 4], 400, 'E106', 'typeが4');

        // type が3 (正常系)
        $template = $this->validTemplate();
        $template['details'] = [$this->validDetail3()];
        $response = $this->request->post('/templates', $template);
        Assert::assertStatusCode200($response->statusCode());
    }

    /**
     * 明細 amount バリデーションエラーテスト (type=1)
     */
    public function testTemplateDetailAmountInvalid(): void
    {
        // amount がない
        $this->assertPostDetail1ErrorUnset('amount', 400, 'E109', 'amountがない');

        // amount が null
        $this->assertPostDetail1Error(['amount' => null], 400, 'E109', 'amountがnull');

        // amount が文字列
        $this->assertPostDetail1Error(['amount' => 'aaa'], 400, 'E101', 'amountが文字列');

        // amount が文字列数字
        $this->assertPostDetail1Error(['amount' => '100'], 400, 'E101', 'amountが文字列数字');
    }

    /**
     * 明細 item バリデーションエラーテスト
     */
    public function testTemplateDetailItemInvalid(): void
    {
        // item がない
        $this->assertPostDetail1ErrorUnset('item', 400, 'E109', 'itemがない');

        // item が null
        $this->assertPostDetail1Error(['item' => null], 400, 'E109', 'itemがnull');

        // item が51文字 (最大50文字)
        $this->assertPostDetail1Error(
            ['item' => 'あいうえおかきくけこさしすせそたちつてとなにぬねのあいうえおかきくけこさしすせそたちつてとなにぬねのあ'],
            400,
            'E105',
            'itemが51文字'
        );

        // item が50文字 (正常系)
        $template = $this->validTemplate();
        $template['details'][0]['item'] = 'あいうえおかきくけこさしすせそたちつてとなにぬねのあいうえおかきくけこさしすせそたちつてとなにぬねの';
        $response = $this->request->post('/templates', $template);
        Assert::assertStatusCode200($response->statusCode());
    }

    /**
     * 明細 kind_element_id バリデーションエラーテスト
     */
    public function testTemplateDetailKindElementIdInvalid(): void
    {
        // kind_element_id がない
        $this->assertPostDetail1ErrorUnset('kind_element_id', 400, 'E109', 'kind_element_idがない');

        // kind_element_id が null
        $this->assertPostDetail1Error(['kind_element_id' => null], 400, 'E109', 'kind_element_idがnull');

        // kind_element_id が文字列
        $this->assertPostDetail1Error(['kind_element_id' => 'aaa'], 400, 'E109', 'kind_element_idが文字列');

        // kind_element_id が文字列数字
        $this->assertPostDetail1Error(['kind_element_id' => '2'], 400, 'E109', 'kind_element_idが文字列数字');
    }

    /**
     * 明細 type=1 purpose_element_id バリデーションエラーテスト
     */
    public function testTemplateDetailType1PurposeElementIdInvalid(): void
    {
        // purpose_element_id がない
        $this->assertPostDetail1ErrorUnset('purpose_element_id', 400, 'E109', 'purpose_element_idがない');

        // purpose_element_id が null
        $this->assertPostDetail1Error(['purpose_element_id' => null], 400, 'E109', 'purpose_element_idがnull');

        // purpose_element_id が文字列
        $this->assertPostDetail1Error(['purpose_element_id' => 'aaa'], 400, 'E109', 'purpose_element_idが文字列');

        // purpose_element_id が文字列数字
        $this->assertPostDetail1Error(['purpose_element_id' => '3'], 400, 'E109', 'purpose_element_idが文字列数字');
    }

    /**
     * 明細 type=1 place_element_id バリデーションエラーテスト
     */
    public function testTemplateDetailType1PlaceElementIdInvalid(): void
    {
        // place_element_id がない
        $this->assertPostDetail1ErrorUnset('place_element_id', 400, 'E109', 'place_element_idがない');

        // place_element_id が null
        $this->assertPostDetail1Error(['place_element_id' => null], 400, 'E109', 'place_element_idがnull');

        // place_element_id が文字列
        $this->assertPostDetail1Error(['place_element_id' => 'aaa'], 400, 'E109', 'place_element_idが文字列');

        // place_element_id が文字列数字
        $this->assertPostDetail1Error(['place_element_id' => '4'], 400, 'E109', 'place_element_idが文字列数字');
    }

    /**
     * 明細 type=1 の移動系フィールドは null でなければならないテスト
     */
    public function testTemplateDetailType1MoveFieldsMustBeNull(): void
    {
        $this->assertPostDetail1Error(['move_before_purpose_id' => 1], 400, 'E106', 'type=1でmove_before_purpose_idがnullでない');
        $this->assertPostDetail1Error(['move_after_purpose_id' => 1], 400, 'E106', 'type=1でmove_after_purpose_idがnullでない');
        $this->assertPostDetail1Error(['move_before_place_id' => 1], 400, 'E106', 'type=1でmove_before_place_idがnullでない');
        $this->assertPostDetail1Error(['move_after_place_id' => 1], 400, 'E106', 'type=1でmove_after_place_idがnullでない');
    }

    /**
     * 明細 type=2 amount は0以上でなければならないテスト
     */
    public function testTemplateDetailType2AmountMustBeNonNegative(): void
    {
        // amount がマイナス
        $this->assertPostDetail2Error(['amount' => -1], 400, 'E102', 'type=2でamountがマイナス');

        // amount が0 (正常系)
        $template = $this->validTemplate();
        $template['details'] = [array_merge($this->validDetail2(), ['amount' => 0])];
        $response = $this->request->post('/templates', $template);
        Assert::assertStatusCode200($response->statusCode());
    }

    /**
     * 明細 type=2 move_before_purpose_id バリデーションエラーテスト
     */
    public function testTemplateDetailType2MoveBeforePurposeIdInvalid(): void
    {
        // move_before_purpose_id がない
        $this->assertPostDetail2ErrorUnset('move_before_purpose_id', 400, 'E109', 'move_before_purpose_idがない');

        // move_before_purpose_id が null
        $this->assertPostDetail2Error(['move_before_purpose_id' => null], 400, 'E109', 'move_before_purpose_idがnull');

        // move_before_purpose_id が文字列
        $this->assertPostDetail2Error(['move_before_purpose_id' => 'aaa'], 400, 'E109', 'move_before_purpose_idが文字列');
    }

    /**
     * 明細 type=2 move_after_purpose_id バリデーションエラーテスト
     */
    public function testTemplateDetailType2MoveAfterPurposeIdInvalid(): void
    {
        // move_after_purpose_id がない
        $this->assertPostDetail2ErrorUnset('move_after_purpose_id', 400, 'E109', 'move_after_purpose_idがない');

        // move_after_purpose_id が null
        $this->assertPostDetail2Error(['move_after_purpose_id' => null], 400, 'E109', 'move_after_purpose_idがnull');

        // move_after_purpose_id が文字列
        $this->assertPostDetail2Error(['move_after_purpose_id' => 'aaa'], 400, 'E109', 'move_after_purpose_idが文字列');
    }

    /**
     * 明細 type=2 の purpose/place は null でなければならないテスト
     */
    public function testTemplateDetailType2PurposePlaceMustBeNull(): void
    {
        $this->assertPostDetail2Error(['purpose_element_id' => 3], 400, 'E106', 'type=2でpurpose_element_idがnullでない');
        $this->assertPostDetail2Error(['place_element_id' => 4], 400, 'E106', 'type=2でplace_element_idがnullでない');
    }

    /**
     * 明細 type=2 の場所移動系フィールドは null でなければならないテスト
     */
    public function testTemplateDetailType2MovePlacesMustBeNull(): void
    {
        $this->assertPostDetail2Error(['move_before_place_id' => 1], 400, 'E106', 'type=2でmove_before_place_idがnullでない');
        $this->assertPostDetail2Error(['move_after_place_id' => 1], 400, 'E106', 'type=2でmove_after_place_idがnullでない');
    }

    /**
     * 明細 type=3 amount は0以上でなければならないテスト
     */
    public function testTemplateDetailType3AmountMustBeNonNegative(): void
    {
        // amount がマイナス
        $this->assertPostDetail3Error(['amount' => -1], 400, 'E102', 'type=3でamountがマイナス');

        // amount が0 (正常系)
        $template = $this->validTemplate();
        $template['details'] = [array_merge($this->validDetail3(), ['amount' => 0])];
        $response = $this->request->post('/templates', $template);
        Assert::assertStatusCode200($response->statusCode());
    }

    /**
     * 明細 type=3 move_before_place_id バリデーションエラーテスト
     */
    public function testTemplateDetailType3MoveBeforePlaceIdInvalid(): void
    {
        // move_before_place_id がない
        $this->assertPostDetail3ErrorUnset('move_before_place_id', 400, 'E109', 'move_before_place_idがない');

        // move_before_place_id が null
        $this->assertPostDetail3Error(['move_before_place_id' => null], 400, 'E109', 'move_before_place_idがnull');

        // move_before_place_id が文字列
        $this->assertPostDetail3Error(['move_before_place_id' => 'aaa'], 400, 'E109', 'move_before_place_idが文字列');
    }

    /**
     * 明細 type=3 move_after_place_id バリデーションエラーテスト
     */
    public function testTemplateDetailType3MoveAfterPlaceIdInvalid(): void
    {
        // move_after_place_id がない
        $this->assertPostDetail3ErrorUnset('move_after_place_id', 400, 'E109', 'move_after_place_idがない');

        // move_after_place_id が null
        $this->assertPostDetail3Error(['move_after_place_id' => null], 400, 'E109', 'move_after_place_idがnull');

        // move_after_place_id が文字列
        $this->assertPostDetail3Error(['move_after_place_id' => 'aaa'], 400, 'E109', 'move_after_place_idが文字列');
    }

    /**
     * 明細 type=3 の purpose/place は null でなければならないテスト
     */
    public function testTemplateDetailType3PurposePlaceMustBeNull(): void
    {
        $this->assertPostDetail3Error(['purpose_element_id' => 3], 400, 'E106', 'type=3でpurpose_element_idがnullでない');
        $this->assertPostDetail3Error(['place_element_id' => 4], 400, 'E106', 'type=3でplace_element_idがnullでない');
    }

    /**
     * 明細 type=3 の予算移動系フィールドは null でなければならないテスト
     */
    public function testTemplateDetailType3MovePurposesMustBeNull(): void
    {
        $this->assertPostDetail3Error(['move_before_purpose_id' => 1], 400, 'E106', 'type=3でmove_before_purpose_idがnullでない');
        $this->assertPostDetail3Error(['move_after_purpose_id' => 1], 400, 'E106', 'type=3でmove_after_purpose_idがnullでない');
    }

    /**
     * 外部キー制約違反テスト
     */
    public function testTemplateForeignKeyViolation(): void
    {
        // kind_element_id が存在しない
        $this->assertPostDetail1Error(['kind_element_id' => 999999], 409, 'E302', 'kind_element_idが存在しない');

        // purpose_element_id が存在しない (type=1)
        $this->assertPostDetail1Error(['purpose_element_id' => 999999], 409, 'E302', 'purpose_element_idが存在しない');

        // place_element_id が存在しない (type=1)
        $this->assertPostDetail1Error(['place_element_id' => 999999], 409, 'E302', 'place_element_idが存在しない');

        // move_before_purpose_id が存在しない (type=2)
        $this->assertPostDetail2Error(['move_before_purpose_id' => 999999], 409, 'E302', 'move_before_purpose_idが存在しない');

        // move_after_purpose_id が存在しない (type=2)
        $this->assertPostDetail2Error(['move_after_purpose_id' => 999999], 409, 'E302', 'move_after_purpose_idが存在しない');

        // move_before_place_id が存在しない (type=3)
        $this->assertPostDetail3Error(['move_before_place_id' => 999999], 409, 'E302', 'move_before_place_idが存在しない');

        // move_after_place_id が存在しない (type=3)
        $this->assertPostDetail3Error(['move_after_place_id' => 999999], 409, 'E302', 'move_after_place_idが存在しない');
    }

    private function validTemplate(): array
    {
        return [
            'name' => 'テストテンプレート',
            'details' => [
                $this->validDetail1(),
                $this->validDetail2(),
            ],
        ];
    }

    private function validDetail1(): array
    {
        return [
            'type' => 1,
            'amount' => 1000,
            'item' => 'テスト収支',
            'kind_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
        ];
    }

    private function validDetail2(): array
    {
        return [
            'type' => 2,
            'amount' => 500,
            'item' => 'テスト予算移動',
            'kind_element_id' => 2,
            'move_before_purpose_id' => 2,
            'move_after_purpose_id' => 5,
        ];
    }

    private function validDetail3(): array
    {
        return [
            'type' => 3,
            'amount' => 300,
            'item' => 'テスト場所移動',
            'kind_element_id' => 2,
            'move_before_place_id' => 4,
            'move_after_place_id' => 5,
        ];
    }

    private function assertDetailFields(array $detail, array $expected, string $prefix): void
    {
        Assert::assertSame($expected['type'], $detail['type'], "{$prefix}の type");
        Assert::assertSame($expected['amount'], $detail['amount'], "{$prefix}の amount");
        Assert::assertSame($expected['item'], $detail['item'], "{$prefix}の item");
        Assert::assertSame($expected['kind_element_id'], $detail['kind_element_id'], "{$prefix}の kind_element_id");
        Assert::assertSame($expected['purpose_element_id'] ?? null, $detail['purpose_element_id'], "{$prefix}の purpose_element_id");
        Assert::assertSame($expected['place_element_id'] ?? null, $detail['place_element_id'], "{$prefix}の place_element_id");
        Assert::assertSame($expected['move_before_purpose_id'] ?? null, $detail['move_before_purpose_id'], "{$prefix}の move_before_purpose_id");
        Assert::assertSame($expected['move_after_purpose_id'] ?? null, $detail['move_after_purpose_id'], "{$prefix}の move_after_purpose_id");
        Assert::assertSame($expected['move_before_place_id'] ?? null, $detail['move_before_place_id'], "{$prefix}の move_before_place_id");
        Assert::assertSame($expected['move_after_place_id'] ?? null, $detail['move_after_place_id'], "{$prefix}の move_after_place_id");
    }

    private function assertPostError(array $overrides, int $statusCode, string $errorCode, string $message): void
    {
        $template = array_merge($this->validTemplate(), $overrides);
        $response = $this->request->post('/templates', $template);
        $this->assertErrorResponse($response, $statusCode, $errorCode, $message);
    }

    private function assertPostErrorUnset(string $field, int $statusCode, string $errorCode, string $message): void
    {
        $template = $this->validTemplate();
        unset($template[$field]);
        $response = $this->request->post('/templates', $template);
        $this->assertErrorResponse($response, $statusCode, $errorCode, $message);
    }

    private function assertPostDetail1Error(array $overrides, int $statusCode, string $errorCode, string $message): void
    {
        $template = $this->validTemplate();
        $template['details'] = [array_merge($this->validDetail1(), $overrides)];
        $response = $this->request->post('/templates', $template);
        $this->assertErrorResponse($response, $statusCode, $errorCode, $message);
    }

    private function assertPostDetail1ErrorUnset(string $field, int $statusCode, string $errorCode, string $message): void
    {
        $template = $this->validTemplate();
        $detail = $this->validDetail1();
        unset($detail[$field]);
        $template['details'] = [$detail];
        $response = $this->request->post('/templates', $template);
        $this->assertErrorResponse($response, $statusCode, $errorCode, $message);
    }

    private function assertPostDetail2Error(array $overrides, int $statusCode, string $errorCode, string $message): void
    {
        $template = $this->validTemplate();
        $template['details'] = [array_merge($this->validDetail2(), $overrides)];
        $response = $this->request->post('/templates', $template);
        $this->assertErrorResponse($response, $statusCode, $errorCode, $message);
    }

    private function assertPostDetail2ErrorUnset(string $field, int $statusCode, string $errorCode, string $message): void
    {
        $template = $this->validTemplate();
        $detail = $this->validDetail2();
        unset($detail[$field]);
        $template['details'] = [$detail];
        $response = $this->request->post('/templates', $template);
        $this->assertErrorResponse($response, $statusCode, $errorCode, $message);
    }

    private function assertPostDetail3Error(array $overrides, int $statusCode, string $errorCode, string $message): void
    {
        $template = $this->validTemplate();
        $template['details'] = [array_merge($this->validDetail3(), $overrides)];
        $response = $this->request->post('/templates', $template);
        $this->assertErrorResponse($response, $statusCode, $errorCode, $message);
    }

    private function assertPostDetail3ErrorUnset(string $field, int $statusCode, string $errorCode, string $message): void
    {
        $template = $this->validTemplate();
        $detail = $this->validDetail3();
        unset($detail[$field]);
        $template['details'] = [$detail];
        $response = $this->request->post('/templates', $template);
        $this->assertErrorResponse($response, $statusCode, $errorCode, $message);
    }

    private function assertErrorResponse($response, int $statusCode, string $errorCode, string $message): void
    {
        $assertMethod = 'assertStatusCode' . $statusCode;
        Assert::$assertMethod($response->statusCode());
        Assert::assertSame($errorCode, $response->jsonBody()['code'], $message);
    }
}
