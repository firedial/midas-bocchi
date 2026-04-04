<?php

require_once __DIR__ . '/../TestRunner/TestCase.php';

class TemplateTest extends TestCase
{
    /**
     * シナリオCRUDテスト
     */
    public function testTemplateCRUD(): void
    {
        $createData = $this->validTemplate();
        $updateData = [
            'name' => 'シナリオ更新後',
            'details' => [
                [
                    'type' => 1,
                    'amount' => -200,
                    'item' => '更新収支',
                    'type_element_id' => 12,
                    'purpose_element_id' => 13,
                    'place_element_id' => 14,
                    'move_attribute' => null,
                    'move_before_id' => null,
                    'move_after_id' => null,
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
     * シナリオ一覧取得テスト
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
     * シナリオ一覧レスポンスボディテスト（明細を含まない）
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

        Assert::assertSame(true, !is_null($found), '一覧に登録したシナリオが存在する');
        Assert::assertSame(false, isset($found['details']), '一覧にdetailsは含まれない');
    }

    /**
     * シナリオ詳細レスポンスボディテスト（seq昇順）
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
     * 存在しないシナリオのテスト
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
     * シナリオ名バリデーションエラーテスト
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

        // type が範囲外 (3)
        $this->assertPostDetail1Error(['type' => 3], 400, 'E106', 'typeが3');
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

        // amount が0 (type=1)
        $this->assertPostDetail1Error(['amount' => 0], 400, 'E102', 'amountが0');
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
     * 明細 type_element_id バリデーションエラーテスト
     */
    public function testTemplateDetailTypeElementIdInvalid(): void
    {
        // type_element_id がない
        $this->assertPostDetail1ErrorUnset('type_element_id', 400, 'E109', 'type_element_idがない');

        // type_element_id が null
        $this->assertPostDetail1Error(['type_element_id' => null], 400, 'E109', 'type_element_idがnull');

        // type_element_id が文字列
        $this->assertPostDetail1Error(['type_element_id' => 'aaa'], 400, 'E101', 'type_element_idが文字列');

        // type_element_id が文字列数字
        $this->assertPostDetail1Error(['type_element_id' => '2'], 400, 'E101', 'type_element_idが文字列数字');
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
        $this->assertPostDetail1Error(['move_attribute' => 1], 400, 'E106', 'type=1でmove_attributeがnullでない');
        $this->assertPostDetail1Error(['move_before_id' => 1], 400, 'E106', 'type=1でmove_before_idがnullでない');
        $this->assertPostDetail1Error(['move_after_id' => 1], 400, 'E106', 'type=1でmove_after_idがnullでない');
    }

    /**
     * 明細 type=2 amount は正の値でなければならないテスト
     */
    public function testTemplateDetailType2AmountMustBePositive(): void
    {
        // amount が0
        $this->assertPostDetail2Error(['amount' => 0], 400, 'E102', 'type=2でamountが0');

        // amount がマイナス
        $this->assertPostDetail2Error(['amount' => -1], 400, 'E102', 'type=2でamountがマイナス');
    }

    /**
     * 明細 type=2 move_attribute バリデーションエラーテスト
     */
    public function testTemplateDetailType2MoveKindInvalid(): void
    {
        // move_attribute がない
        $this->assertPostDetail2ErrorUnset('move_attribute', 400, 'E109', 'move_attributeがない');

        // move_attribute が null
        $this->assertPostDetail2Error(['move_attribute' => null], 400, 'E109', 'move_attributeがnull');

        // move_attribute が文字列
        $this->assertPostDetail2Error(['move_attribute' => 'aaa'], 400, 'E109', 'move_attributeが文字列');

        // move_attribute が範囲外 (0)
        $this->assertPostDetail2Error(['move_attribute' => 0], 400, 'E109', 'move_attributeが0');

        // move_attribute が範囲外 (3)
        $this->assertPostDetail2Error(['move_attribute' => 3], 400, 'E109', 'move_attributeが3');
    }

    /**
     * 明細 type=2 move_before_id バリデーションエラーテスト
     */
    public function testTemplateDetailType2MoveBeforeIdInvalid(): void
    {
        // move_before_id がない
        $this->assertPostDetail2ErrorUnset('move_before_id', 400, 'E109', 'move_before_idがない');

        // move_before_id が null
        $this->assertPostDetail2Error(['move_before_id' => null], 400, 'E109', 'move_before_idがnull');

        // move_before_id が文字列
        $this->assertPostDetail2Error(['move_before_id' => 'aaa'], 400, 'E109', 'move_before_idが文字列');
    }

    /**
     * 明細 type=2 move_after_id バリデーションエラーテスト
     */
    public function testTemplateDetailType2MoveAfterIdInvalid(): void
    {
        // move_after_id がない
        $this->assertPostDetail2ErrorUnset('move_after_id', 400, 'E109', 'move_after_idがない');

        // move_after_id が null
        $this->assertPostDetail2Error(['move_after_id' => null], 400, 'E109', 'move_after_idがnull');

        // move_after_id が文字列
        $this->assertPostDetail2Error(['move_after_id' => 'aaa'], 400, 'E109', 'move_after_idが文字列');
    }

    /**
     * 明細 type=2 の purpose/place は null でなければならないテスト
     */
    public function testTemplateDetailType2PurposePlaceMustBeNull(): void
    {
        $this->assertPostDetail2Error(['purpose_element_id' => 3], 400, 'E106', 'type=2でpurpose_element_idがnullでない');
        $this->assertPostDetail2Error(['place_element_id' => 4], 400, 'E106', 'type=2でplace_element_idがnullでない');
    }

    private function validTemplate(): array
    {
        return [
            'name' => 'テストシナリオ',
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
            'type_element_id' => 2,
            'purpose_element_id' => 3,
            'place_element_id' => 4,
            'move_attribute' => null,
            'move_before_id' => null,
            'move_after_id' => null,
        ];
    }

    private function validDetail2(): array
    {
        return [
            'type' => 2,
            'amount' => 500,
            'item' => 'テスト移動',
            'type_element_id' => 2,
            'purpose_element_id' => null,
            'place_element_id' => null,
            'move_attribute' => 2,
            'move_before_id' => 2,
            'move_after_id' => 5,
        ];
    }

    private function assertDetailFields(array $detail, array $expected, string $prefix): void
    {
        Assert::assertSame($expected['type'], $detail['type'], "{$prefix}の type");
        Assert::assertSame($expected['amount'], $detail['amount'], "{$prefix}の amount");
        Assert::assertSame($expected['item'], $detail['item'], "{$prefix}の item");
        Assert::assertSame($expected['type_element_id'], $detail['type_element_id'], "{$prefix}の type_element_id");
        Assert::assertSame($expected['purpose_element_id'], $detail['purpose_element_id'], "{$prefix}の purpose_element_id");
        Assert::assertSame($expected['place_element_id'], $detail['place_element_id'], "{$prefix}の place_element_id");
        Assert::assertSame($expected['move_attribute'], $detail['move_attribute'], "{$prefix}の move_attribute");
        Assert::assertSame($expected['move_before_id'], $detail['move_before_id'], "{$prefix}の move_before_id");
        Assert::assertSame($expected['move_after_id'], $detail['move_after_id'], "{$prefix}の move_after_id");
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

    private function assertErrorResponse($response, int $statusCode, string $errorCode, string $message): void
    {
        $assertMethod = 'assertStatusCode' . $statusCode;
        Assert::$assertMethod($response->statusCode());
        Assert::assertSame($errorCode, $response->jsonBody()['code'], $message);
    }
}
