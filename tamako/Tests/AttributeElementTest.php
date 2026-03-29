<?php

require_once __DIR__ . '/../TestRunner/TestCase.php';

class AttributeElementTest extends TestCase
{
    private int $suffix = 10000;

    /**
     * 属性要素一覧取得テスト
     */
    public function testAttributeElementGet(): void
    {
        foreach ($this->attributeNames() as $attribute) {
            $response = $this->request->get('/attribute_elements/' . $attribute);
            Assert::assertStatusCode200($response->statusCode());

            $element = $response->jsonBody()[0];
            Assert::assertSame(true, is_int($element['id']), $attribute . ' idがint');
            Assert::assertSame(true, is_string($element['name']), $attribute . ' nameがstring');
            Assert::assertSame(true, is_string($element['description']), $attribute . ' descriptionがstring');
            Assert::assertSame(true, is_int($element['priority']), $attribute . ' priorityがint');
            Assert::assertSame(true, is_int($element['category_id']), $attribute . ' category_idがint');
        }
    }

    /**
     * 属性要素一覧取得テスト(属性名不正)
     */
    public function testAttributeElementGetInvalidAttributeName(): void
    {
        $response = $this->request->get('/attribute_elements/aaa');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E305', $response->jsonBody()['code'], '属性名不正');
    }

    /**
     * 属性要素CRUDテスト - kind_element
     */
    public function testAttributeElementCRUDKindElement(): void
    {
        $this->assertAttributeElementCRUD('kind_element');
    }

    /**
     * 属性要素CRUDテスト - purpose_element
     */
    public function testAttributeElementCRUDPurposeElement(): void
    {
        $this->assertAttributeElementCRUD('purpose_element');
    }

    /**
     * 属性要素CRUDテスト - place_element
     */
    public function testAttributeElementCRUDPlaceElement(): void
    {
        $this->assertAttributeElementCRUD('place_element');
    }

    /**
     * 属性要素登録テスト（優先度の範囲）
     */
    public function testAttributeElementPostPriority(): void
    {
        foreach ($this->attributeNames() as $attribute) {
            // 0
            $element = $this->validAttributeElement();
            $element['priority'] = 0;
            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode200($response->statusCode());

            // 100
            $element = $this->validAttributeElement();
            $element['priority'] = 100;
            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode200($response->statusCode());

            // -1 は登録できない
            $element = $this->validAttributeElement();
            $element['priority'] = -1;
            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E102', $response->jsonBody()['code'], $attribute . ' priorityが-1');

            // 101 は登録できない
            $element = $this->validAttributeElement();
            $element['priority'] = 101;
            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E102', $response->jsonBody()['code'], $attribute . ' priorityが101');
        }
    }

    /**
     * 属性要素登録バリデーションエラーテスト（優先度）
     */
    public function testAttributeElementPostPriorityInvalid(): void
    {
        $this->assertPostErrorForAll(['priority' => ''], 400, 'E101', 'priorityが空');
        $this->assertPostErrorForAll(['priority' => null], 400, 'E101', 'priorityがnull');
        $this->assertPostErrorUnsetForAll('priority', 400, 'E109', 'priorityがない');
        $this->assertPostErrorForAll(['priority' => '10'], 400, 'E101', 'priorityが文字列');
    }

    /**
     * 属性要素登録バリデーションエラーテスト（名前）
     */
    public function testAttributeElementPostNameInvalid(): void
    {
        $this->assertPostErrorForAll(['name' => ''], 400, 'E105', 'nameが空');
        $this->assertPostErrorForAll(['name' => null], 400, 'E101', 'nameがnull');
        $this->assertPostErrorUnsetForAll('name', 400, 'E109', 'nameがない');
        $this->assertPostErrorForAll(['name' => 'Aaa'], 400, 'E103', 'nameが大文字始まり');
        $this->assertPostErrorForAll(['name' => 'あああ'], 400, 'E103', 'nameがマルチバイト');
    }

    /**
     * 属性要素登録バリデーションエラーテスト（名前の長さ）
     */
    public function testAttributeElementPostNameLength(): void
    {
        foreach ($this->attributeNames() as $attribute) {
            // 20文字は登録できる
            $element = $this->validAttributeElement();
            $element['name'] = 'abcd_abAB_efgt_' . str_pad((string)$this->suffix, 5, '0', STR_PAD_LEFT);
            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode200($response->statusCode());

            // 21文字は登録できない
            $element = $this->validAttributeElement();
            $element['name'] = 'abcd_abAB_efgt_a' . str_pad((string)$this->suffix, 5, '0', STR_PAD_LEFT);
            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E105', $response->jsonBody()['code'], $attribute . ' nameが長い');
        }
    }

    /**
     * 属性要素登録バリデーションエラーテスト（説明）
     */
    public function testAttributeElementPostDescriptionInvalid(): void
    {
        $this->assertPostErrorForAll(['description' => ''], 400, 'E105', 'descriptionが空');
        $this->assertPostErrorForAll(['description' => null], 400, 'E101', 'descriptionがnull');
        $this->assertPostErrorUnsetForAll('description', 400, 'E109', 'descriptionがない');
    }

    /**
     * 属性要素登録バリデーションエラーテスト（説明長さ）
     */
    public function testAttributeElementPostDescriptionLength(): void
    {
        foreach ($this->attributeNames() as $attribute) {
            // 20文字
            $element = $this->validAttributeElement();
            $element['description'] = 'あいうえおかきくけこさしすせそたちつてと';
            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode200($response->statusCode());

            // 21文字
            $element = $this->validAttributeElement();
            $element['description'] = 'あいうえおかきくけこさしすせそたちつてとな';
            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E105', $response->jsonBody()['code'], $attribute . ' descriptionが長い');
        }
    }

    /**
     * 属性要素登録バリデーションエラーテスト（親カテゴリ）
     */
    public function testAttributeElementPostCategoryIdInvalid(): void
    {
        $this->assertPostErrorUnsetForAll('category_id', 400, 'E109', 'category_idがない');
        $this->assertPostErrorForAll(['category_id' => null], 400, 'E101', 'category_idがnull');
        $this->assertPostErrorForAll(['category_id' => ''], 400, 'E101', 'category_idが空文字列');
        $this->assertPostErrorForAll(['category_id' => '10'], 400, 'E101', 'category_idが文字列');
        $this->assertPostErrorForAll(['category_id' => 1], 400, 'E108', 'category_idが移動ID');
    }

    /**
     * 属性要素登録バリデーションエラーテスト（対象の親カテゴリがない）
     */
    public function testAttributeElementPostCategoryIdNotExist(): void
    {
        $this->assertPostErrorForAll(['category_id' => 99999], 409, 'E302', 'category_idが存在しない');
    }

    /**
     * 属性要素登録バリデーションエラーテスト（名前が重複）
     */
    public function testAttributeElementPostNameDuplicate(): void
    {
        foreach ($this->attributeNames() as $attribute) {
            $name = 'dup_post' . $this->suffix;

            $element = $this->validAttributeElement();
            $element['name'] = $name;
            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode200($response->statusCode());

            $element = $this->validAttributeElement();
            $element['name'] = $name;
            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode409($response->statusCode());
            Assert::assertSame('E304', $response->jsonBody()['code'], $attribute . ' name が重複');
        }
    }

    /**
     * 属性要素更新バリデーションエラーテスト（優先度）
     */
    public function testAttributeElementPutPriorityInvalid(): void
    {
        $this->assertPutErrorForAll(['priority' => ''], 400, 'E101', 'priorityが空');
        $this->assertPutErrorUnsetForAll('priority', 400, 'E109', 'priorityがない');
        $this->assertPutErrorForAll(['priority' => null], 400, 'E101', 'priorityがnull');
        $this->assertPutErrorForAll(['priority' => '10'], 400, 'E101', 'priorityが文字列');
    }

    /**
     * 属性要素更新バリデーションエラーテスト（名前）
     */
    public function testAttributeElementPutNameInvalid(): void
    {
        $this->assertPutErrorForAll(['name' => ''], 400, 'E105', 'nameが空');
        $this->assertPutErrorForAll(['name' => null], 400, 'E101', 'nameがnull');
        $this->assertPutErrorUnsetForAll('name', 400, 'E109', 'nameがない');
        $this->assertPutErrorForAll(['name' => 'Aaa'], 400, 'E103', 'nameが大文字始まり');
        $this->assertPutErrorForAll(['name' => 'あああ'], 400, 'E103', 'nameがマルチバイト');
    }

    /**
     * 属性要素更新バリデーションエラーテスト（名前の長さ）
     */
    public function testAttributeElementPutNameLength(): void
    {
        foreach ($this->attributeNames() as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            // 20文字は登録できる
            $element = $this->validAttributeElement();
            $element['name'] = 'aput_abAB_efgt_' . str_pad((string)$this->suffix, 5, '0', STR_PAD_LEFT);
            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode200($response->statusCode());

            // 21文字は登録できない
            $element = $this->validAttributeElement();
            $element['name'] = 'aput_abAB_efgt_a' . str_pad((string)$this->suffix, 5, '0', STR_PAD_LEFT);
            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E105', $response->jsonBody()['code'], $attribute . ' nameが長い');
        }
    }

    /**
     * 属性要素更新バリデーションエラーテスト（説明）
     */
    public function testAttributeElementPutDescriptionInvalid(): void
    {
        $this->assertPutErrorForAll(['description' => ''], 400, 'E105', 'descriptionが空');
        $this->assertPutErrorForAll(['description' => null], 400, 'E101', 'descriptionがnull');
        $this->assertPutErrorUnsetForAll('description', 400, 'E109', 'descriptionがない');
    }

    /**
     * 属性要素更新バリデーションエラーテスト（説明長さ）
     */
    public function testAttributeElementPutDescriptionLength(): void
    {
        foreach ($this->attributeNames() as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            // 20文字
            $element = $this->validAttributeElement();
            $element['description'] = 'あいうえおかきくけこさしすせそたちつてと';
            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode200($response->statusCode());

            // 21文字
            $element = $this->validAttributeElement();
            $element['description'] = 'あいうえおかきくけこさしすせそたちつてとな';
            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E105', $response->jsonBody()['code'], $attribute . ' descriptionが長い');
        }
    }

    /**
     * 属性要素更新テスト（優先度の範囲）
     */
    public function testAttributeElementPutPriorityRange(): void
    {
        foreach ($this->attributeNames() as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            // 0
            $element = $this->validAttributeElement();
            $element['priority'] = 0;
            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode200($response->statusCode());

            // 100
            $element = $this->validAttributeElement();
            $element['priority'] = 100;
            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode200($response->statusCode());

            // -1
            $element = $this->validAttributeElement();
            $element['priority'] = -1;
            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E102', $response->jsonBody()['code'], $attribute . ' priorityが負');

            // 101
            $element = $this->validAttributeElement();
            $element['priority'] = 101;
            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E102', $response->jsonBody()['code'], $attribute . ' priorityが101以上');
        }
    }

    /**
     * 属性要素更新バリデーションエラーテスト（親カテゴリ）
     */
    public function testAttributeElementPutCategoryIdInvalid(): void
    {
        $this->assertPutErrorUnsetForAll('category_id', 400, 'E109', 'category_idがない');
        $this->assertPutErrorForAll(['category_id' => null], 400, 'E101', 'category_idがnull');
        $this->assertPutErrorForAll(['category_id' => ''], 400, 'E101', 'category_idが空文字列');
        $this->assertPutErrorForAll(['category_id' => '10'], 400, 'E101', 'category_idが文字列');
        $this->assertPutErrorForAll(['category_id' => 1], 400, 'E108', 'category_idが移動ID');
    }

    /**
     * 属性要素更新バリデーションエラーテスト（対象の親カテゴリがない）
     */
    public function testAttributeElementPutCategoryIdNotExist(): void
    {
        $this->assertPutErrorForAll(['category_id' => 99999], 409, 'E302', 'category_idが存在しない');
    }

    /**
     * 属性要素更新バリデーションエラーテスト（移動IDレコードの更新）
     */
    public function testAttributeElementPutMove(): void
    {
        foreach ($this->attributeNames() as $attribute) {
            $element = $this->validAttributeElement();
            $response = $this->request->put('/attribute_elements/' . $attribute . '/1', $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E108', $response->jsonBody()['code'], $attribute . ' 移動IDの更新');
        }
    }

    /**
     * 属性要素更新バリデーションエラーテスト（名前が重複）
     */
    public function testAttributeElementPutNameDuplicate(): void
    {
        foreach ($this->attributeNames() as $attribute) {
            // 重複させるレコード
            $name = 'dup_put' . $this->suffix;
            $element = $this->validAttributeElement();
            $element['name'] = $name;
            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode200($response->statusCode());

            // 更新するレコードの作成
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            // 更新
            $element = $this->validAttributeElement();
            $element['name'] = $name;
            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode409($response->statusCode());
            Assert::assertSame('E304', $response->jsonBody()['code'], $attribute . ' name が重複');
        }
    }

    /**
     * 認証なし
     */
    public function testAttributeElementWithoutAuth(): void
    {
        $noSessionRequest = new Request();

        foreach ($this->attributeNames() as $attribute) {
            $response = $noSessionRequest->get('/attribute_elements/' . $attribute);
            Assert::assertStatusCode401($response->statusCode());
            Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し一覧取得 ' . $attribute);

            $response = $noSessionRequest->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode401($response->statusCode());
            Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し登録 ' . $attribute);

            $response = $noSessionRequest->get('/attribute_elements/' . $attribute . '/10');
            Assert::assertStatusCode401($response->statusCode());
            Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し取得 ' . $attribute);

            $response = $noSessionRequest->put('/attribute_elements/' . $attribute . '/10', $this->validAttributeElement());
            Assert::assertStatusCode401($response->statusCode());
            Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し更新 ' . $attribute);
        }
    }

    private function attributeNames(): array
    {
        return ['kind_element', 'purpose_element', 'place_element'];
    }

    private function assertAttributeElementCRUD(string $attribute): void
    {
        $createData = [
            'name' => 'hoge' . $this->suffix,
            'description' => '説明',
            'priority' => 50,
            'category_id' => 2,
        ];
        $updateData = [
            'name' => 'fuga' . $this->suffix,
            'description' => '説明更新後',
            'priority' => 100,
            'category_id' => 3,
        ];

        // 登録
        $response = $this->request->post('/attribute_elements/' . $attribute, $createData);
        Assert::assertStatusCode200($response->statusCode());
        $this->assertElementFields($response->jsonBody(), $createData, $attribute . ' 登録後');

        $id = $response->jsonBody()['id'];

        // 個別取得
        $response = $this->request->get('/attribute_elements/' . $attribute . '/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $this->assertElementFields($response->jsonBody(), $createData, $attribute . ' 取得後');

        // 更新
        $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $updateData);
        Assert::assertStatusCode200($response->statusCode());
        $this->assertElementFields($response->jsonBody(), $updateData, $attribute . ' 更新後');

        // 更新後の値を確認
        $response = $this->request->get('/attribute_elements/' . $attribute . '/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $this->assertElementFields($response->jsonBody(), $updateData, $attribute . ' 更新確認後');
    }

    /**
     * 要素の各フィールドをアサートする
     */
    private function assertElementFields(array $element, array $expected, string $prefix): void
    {
        Assert::assertSame($expected['name'], $element['name'], "{$prefix}の name");
        Assert::assertSame($expected['description'], $element['description'], "{$prefix}の description");
        Assert::assertSame($expected['priority'], $element['priority'], "{$prefix}の priority");
        Assert::assertSame($expected['category_id'], $element['category_id'], "{$prefix}の category_id");
    }

    /**
     * 全属性タイプで POST エラーをアサートする（フィールド上書き）
     */
    private function assertPostErrorForAll(array $overrides, int $statusCode, string $errorCode, string $message): void
    {
        foreach ($this->attributeNames() as $attribute) {
            $element = array_merge($this->validAttributeElement(), $overrides);
            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            $this->assertErrorResponse($response, $statusCode, $errorCode, $attribute . ' ' . $message);
        }
    }

    /**
     * 全属性タイプで POST エラーをアサートする（フィールド削除）
     */
    private function assertPostErrorUnsetForAll(string $field, int $statusCode, string $errorCode, string $message): void
    {
        foreach ($this->attributeNames() as $attribute) {
            $element = $this->validAttributeElement();
            unset($element[$field]);
            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            $this->assertErrorResponse($response, $statusCode, $errorCode, $attribute . ' ' . $message);
        }
    }

    /**
     * 全属性タイプで PUT エラーをアサートする（フィールド上書き）
     */
    private function assertPutErrorForAll(array $overrides, int $statusCode, string $errorCode, string $message): void
    {
        foreach ($this->attributeNames() as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $element = array_merge($this->validAttributeElement(), $overrides);
            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            $this->assertErrorResponse($response, $statusCode, $errorCode, $attribute . ' ' . $message);
        }
    }

    /**
     * 全属性タイプで PUT エラーをアサートする（フィールド削除）
     */
    private function assertPutErrorUnsetForAll(string $field, int $statusCode, string $errorCode, string $message): void
    {
        foreach ($this->attributeNames() as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $element = $this->validAttributeElement();
            unset($element[$field]);
            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            $this->assertErrorResponse($response, $statusCode, $errorCode, $attribute . ' ' . $message);
        }
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

    private function validAttributeElement(): array
    {
        $this->suffix += 1;

        return [
            'name' => 'test' . $this->suffix,
            'description' => '説明',
            'priority' => 10,
            'category_id' => 2,
        ];
    }
}
