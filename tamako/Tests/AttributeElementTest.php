<?php

require_once __DIR__ . '/../TestRunner/TestCase.php';

class AttributeElementTest extends TestCase
{
    private int $suffix = 1000;

    /**
     * 属性要素一覧取得テスト
     */
    public function testAttributeElementGet(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $response = $this->request->get('/attribute_elements/' . $attribute);
            Assert::assertStatusCode200($response->statusCode());

            $elements = $response->jsonBody();
            $element = $elements[0];
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
     * 属性要素登録テスト（優先度）
     */
    public function testAttributeElementPostPriority(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
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
     * 属性要素登録バリデーションエラーテスト（優先度が空）
     */
    public function testAttributeElementPostPriorityEmpty(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $element = $this->validAttributeElement();
            $element['priority'] = '';

            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' priorityが空');
        }
    }

    /**
     * 属性要素登録バリデーションエラーテスト（優先度がない）
     */
    public function testAttributeElementPostPriorityMissing(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $element = $this->validAttributeElement();
            unset($element['priority']);

            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' priorityがない');
        }
    }

    /**
     * 属性要素登録バリデーションエラーテスト（優先度が文字列）
     */
    public function testAttributeElementPostPriorityType(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $element = $this->validAttributeElement();
            $element['priority'] = '10';

            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E101', $response->jsonBody()['code'], $attribute . ' priorityが文字列');
        }
    }

    /**
     * 属性要素登録バリデーションエラーテスト（名前が空）
     */
    public function testAttributeElementPostNameEmpty(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $element = $this->validAttributeElement();
            $element['name'] = '';

            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' nameが空');
        }
    }

    /**
     * 属性要素登録バリデーションエラーテスト（名前がない）
     */
    public function testAttributeElementPostNameMissing(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $element = $this->validAttributeElement();
            unset($element['name']);

            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' nameがない');
        }
    }

    /**
     * 属性要素登録バリデーションエラーテスト（名前が大文字から始まる）
     */
    public function testAttributeElementPostNameUppercase(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $element = $this->validAttributeElement();
            $element['name'] = 'Aaa';

            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E103', $response->jsonBody()['code'], $attribute . ' nameが大文字始まり');
        }
    }

    /**
     * 属性要素登録バリデーションエラーテスト（名前がマルチバイト文字）
     */
    public function testAttributeElementPostNameMultibyte(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $element = $this->validAttributeElement();
            $element['name'] = 'あああ';

            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E103', $response->jsonBody()['code'], $attribute . ' nameがマルチバイト');
        }
    }

    /**
     * 属性要素登録バリデーションエラーテスト（説明が空）
     */
    public function testAttributeElementPostDescriptionEmpty(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $element = $this->validAttributeElement();
            $element['description'] = '';

            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' descriptionが空');
        }
    }

    /**
     * 属性要素登録バリデーションエラーテスト（説明がない）
     */
    public function testAttributeElementPostDescriptionMissing(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $element = $this->validAttributeElement();
            unset($element['description']);

            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' descriptionがない');
        }
    }

    /**
     * 属性要素登録バリデーションエラーテスト（親カテゴリなし）
     */
    public function testAttributeElementPostCategoryIdMissing(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $element = $this->validAttributeElement();
            unset($element['category_id']);

            $response = $this->request->post('/attribute_elements/' . $attribute, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' category_idがない');
        }
    }

    /**
     * 属性要素更新バリデーションエラーテスト（優先度が空）
     */
    public function testAttributeElementPutPriorityEmpty(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $element = $this->validAttributeElement();
            $element['priority'] = '';

            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' priorityが空');
        }
    }

    /**
     * 属性要素更新バリデーションエラーテスト（優先度がない）
     */
    public function testAttributeElementPutPriorityMissing(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $element = $this->validAttributeElement();
            unset($element['priority']);

            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' priorityがない');
        }
    }

    /**
     * 属性要素更新バリデーションエラーテスト（優先度が文字列）
     */
    public function testAttributeElementPutPriorityType(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $element = $this->validAttributeElement();
            $element['priority'] = '10';

            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E101', $response->jsonBody()['code'], $attribute . ' priorityが文字列');
        }
    }

    /**
     * 属性要素更新バリデーションエラーテスト（名前が空）
     */
    public function testAttributeElementPutNameEmpty(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $element = $this->validAttributeElement();
            $element['name'] = '';

            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' nameが空');
        }
    }

    /**
     * 属性要素更新バリデーションエラーテスト（名前がない）
     */
    public function testAttributeElementPutNameMissing(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $element = $this->validAttributeElement();
            unset($element['name']);

            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' nameがない');
        }
    }

    /**
     * 属性要素更新バリデーションエラーテスト（名前が大文字から始まる）
     */
    public function testAttributeElementPutNameUppercase(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $element = $this->validAttributeElement();
            $element['name'] = 'Aaa';

            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E103', $response->jsonBody()['code'], $attribute . ' nameが大文字始まり');
        }
    }

    /**
     * 属性要素更新バリデーションエラーテスト（名前がマルチバイト文字）
     */
    public function testAttributeElementPutNameMultibyte(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $element = $this->validAttributeElement();
            $element['name'] = 'あああ';

            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E103', $response->jsonBody()['code'], $attribute . ' nameがマルチバイト');
        }
    }

    /**
     * 属性要素更新バリデーションエラーテスト（説明が空）
     */
    public function testAttributeElementPutDescriptionEmpty(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $element = $this->validAttributeElement();
            $element['description'] = '';

            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' descriptionが空');
        }
    }

    /**
     * 属性要素更新バリデーションエラーテスト（説明がない）
     */
    public function testAttributeElementPutDescriptionMissing(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $element = $this->validAttributeElement();
            unset($element['description']);

            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' descriptionがない');
        }
    }

    /**
     * 属性要素更新バリデーションエラーテスト（優先度）
     */
    public function testAttributeElementPutPriorityNegative(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
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
     * 属性要素更新バリデーションエラーテスト（親カテゴリなし）
     */
    public function testAttributeElementPutCategoryIdMissing(): void
    {
        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_elements/' . $attribute, $this->validAttributeElement());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $element = $this->validAttributeElement();
            unset($element['category_id']);

            $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, $element);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' category_idがない');
        }
    }

    /**
     * 認証なし
     */
    public function testAttributeElementWithoutAuth(): void
    {
        $noSessionRequest = new Request();

        $attributes = ['kind_element', 'purpose_element', 'place_element'];

        foreach ($attributes as $attribute) {
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

    private function assertAttributeElementCRUD(string $attribute): void
    {
        // 登録
        $response = $this->request->post('/attribute_elements/' . $attribute, [
            'name' => 'hoge' . $this->suffix,
            'description' => '説明',
            'priority' => 50,
            'category_id' => 2,
        ]);
        Assert::assertStatusCode200($response->statusCode());
        $element = $response->jsonBody();
        Assert::assertSame('hoge' . $this->suffix, $element['name'], $attribute . ' 登録後の name');
        Assert::assertSame('説明', $element['description'], $attribute . ' 登録後の description');
        Assert::assertSame(50, $element['priority'], $attribute . ' 登録後の priority');
        Assert::assertSame(2, $element['category_id'], $attribute . ' 登録後の category_id');

        $id = $element['id'];

        // 個別取得
        $response = $this->request->get('/attribute_elements/' . $attribute . '/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $element = $response->jsonBody();
        Assert::assertSame('hoge' . $this->suffix, $element['name'], $attribute . ' 取得後の name');
        Assert::assertSame('説明', $element['description'], $attribute . ' 取得後の description');
        Assert::assertSame(50, $element['priority'], $attribute . ' 取得後の priority');
        Assert::assertSame(2, $element['category_id'], $attribute . ' 取得後の category_id');

        // 更新
        $response = $this->request->put('/attribute_elements/' . $attribute . '/' . $id, [
            'name' => 'fuga' . $this->suffix,
            'description' => '説明更新後',
            'priority' => 100,
            'category_id' => 3,
        ]);
        Assert::assertStatusCode200($response->statusCode());
        $element = $response->jsonBody();
        Assert::assertSame('fuga' . $this->suffix, $element['name'], $attribute . ' 更新後の name');
        Assert::assertSame('説明更新後', $element['description'], $attribute . ' 更新後の description');
        Assert::assertSame(100, $element['priority'], $attribute . ' 更新後の priority');
        Assert::assertSame(3, $element['category_id'], $attribute . ' 更新後の category_id');

        // 更新後の値を確認
        $response = $this->request->get('/attribute_elements/' . $attribute . '/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $element = $response->jsonBody();
        Assert::assertSame('fuga' . $this->suffix, $element['name'], $attribute . ' 更新確認後の name');
        Assert::assertSame('説明更新後', $element['description'], $attribute . ' 更新確認後の description');
        Assert::assertSame(100, $element['priority'], $attribute . ' 更新確認後の priority');
        Assert::assertSame(3, $element['category_id'], $attribute . ' 更新確認後の category_id');
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
