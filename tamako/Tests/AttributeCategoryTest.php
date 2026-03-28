<?php

require_once __DIR__ . '/../TestRunner/TestCase.php';

class AttributeCategoryTest extends TestCase
{
    private int $suffix = 1100;

    /**
     * 属性カテゴリー一覧取得テスト
     */
    public function testAttributeCategoryGet(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $response = $this->request->get('/attribute_categories/' . $attribute);
            Assert::assertStatusCode200($response->statusCode());

            $categories = $response->jsonBody();
            $category = $categories[0];
            Assert::assertSame(true, is_int($category['id']), $attribute . ' idがint');
            Assert::assertSame(true, is_string($category['name']), $attribute . ' nameがstring');
            Assert::assertSame(true, is_string($category['description']), $attribute . ' descriptionがstring');
        }
    }

    /**
     * 属性カテゴリー一覧取得テスト(属性名不正)
     */
    public function testAttributeCategoryGetInvalidAttributeName(): void
    {
        $response = $this->request->get('/attribute_categories/aaa');
        Assert::assertStatusCode404($response->statusCode());
        Assert::assertSame('E305', $response->jsonBody()['code'], '属性名不正');
    }

    /**
     * 属性カテゴリーCRUDテスト - kind_category
     */
    public function testAttributeCategoryCRUDKindCategory(): void
    {
        $this->assertAttributeCategoryCRUD('kind_category');
    }

    /**
     * 属性カテゴリーCRUDテスト - purpose_category
     */
    public function testAttributeCategoryCRUDPurposeCategory(): void
    {
        $this->assertAttributeCategoryCRUD('purpose_category');
    }

    /**
     * 属性カテゴリーCRUDテスト - place_category
     */
    public function testAttributeCategoryCRUDPlaceCategory(): void
    {
        $this->assertAttributeCategoryCRUD('place_category');
    }

    /**
     * 属性カテゴリー登録バリデーションエラーテスト（名前が空）
     */
    public function testAttributeCategoryPostNameEmpty(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $category = $this->validAttributeCategory();
            $category['name'] = '';

            $response = $this->request->post('/attribute_categories/' . $attribute, $category);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' nameが空');
        }
    }

    /**
     * 属性カテゴリー登録バリデーションエラーテスト（名前がない）
     */
    public function testAttributeCategoryPostNameMissing(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $category = $this->validAttributeCategory();
            unset($category['name']);

            $response = $this->request->post('/attribute_categories/' . $attribute, $category);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' nameがない');
        }
    }

    /**
     * 属性カテゴリー登録バリデーションエラーテスト（名前の長さ）
     */
    public function testAttributeCategoryPostNameLength(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            // 20文字は登録できる
            $category = $this->validAttributeCategory();
            $category['name'] = 'Abcd_abAB_efgt_' . str_pad((string)$this->suffix, 5, '0', STR_PAD_LEFT);
            $response = $this->request->post('/attribute_categories/' . $attribute, $category);
            Assert::assertStatusCode200($response->statusCode());

            // 21文字は登録できない
            $category = $this->validAttributeCategory();
            $category['name'] = 'Abcd_abAB_efgt_a' . str_pad((string)$this->suffix, 5, '0', STR_PAD_LEFT);
            $response = $this->request->post('/attribute_categories/' . $attribute, $category);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E105', $response->jsonBody()['code'], $attribute . ' nameが長い');
        }
    }

    /**
     * 属性カテゴリー登録バリデーションエラーテスト（名前が小文字から始まる）
     */
    public function testAttributeCategoryPostNameLowercase(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $category = $this->validAttributeCategory();
            $category['name'] = 'aaa';

            $response = $this->request->post('/attribute_categories/' . $attribute, $category);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E103', $response->jsonBody()['code'], $attribute . ' nameが小文字始まり');
        }
    }

    /**
     * 属性カテゴリー登録バリデーションエラーテスト（名前がマルチバイト文字）
     */
    public function testAttributeCategoryPostNameMultibyte(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $category = $this->validAttributeCategory();
            $category['name'] = 'あああ';

            $response = $this->request->post('/attribute_categories/' . $attribute, $category);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E103', $response->jsonBody()['code'], $attribute . ' nameがマルチバイト');
        }
    }

    /**
     * 属性カテゴリー登録バリデーションエラーテスト（説明が空）
     */
    public function testAttributeCategoryPostDescriptionEmpty(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $category = $this->validAttributeCategory();
            $category['description'] = '';

            $response = $this->request->post('/attribute_categories/' . $attribute, $category);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' descriptionが空');
        }
    }

    /**
     * 属性カテゴリー登録バリデーションエラーテスト（説明がない）
     */
    public function testAttributeCategoryPostDescriptionMissing(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $category = $this->validAttributeCategory();
            unset($category['description']);

            $response = $this->request->post('/attribute_categories/' . $attribute, $category);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' descriptionがない');
        }
    }

    /**
     * 属性カテゴリー更新バリデーションエラーテスト（名前が空）
     */
    public function testAttributeCategoryPutNameEmpty(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_categories/' . $attribute, $this->validAttributeCategory());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $category = $this->validAttributeCategory();
            $category['name'] = '';

            $response = $this->request->put('/attribute_categories/' . $attribute . '/' . $id, $category);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' nameが空');
        }
    }

    /**
     * 属性カテゴリー更新バリデーションエラーテスト（名前がない）
     */
    public function testAttributeCategoryPutNameMissing(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_categories/' . $attribute, $this->validAttributeCategory());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $category = $this->validAttributeCategory();
            unset($category['name']);

            $response = $this->request->put('/attribute_categories/' . $attribute . '/' . $id, $category);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' nameがない');
        }
    }

    /**
     * 属性カテゴリー更新バリデーションエラーテスト（名前の長さ）
     */
    public function testAttributeCategoryPutNameLength(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_categories/' . $attribute, $this->validAttributeCategory());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            // 20文字は登録できる
            $category = $this->validAttributeCategory();
            $category['name'] = 'Aput_abAB_efgt_' . str_pad((string)$this->suffix, 5, '0', STR_PAD_LEFT);
            $response = $this->request->put('/attribute_categories/' . $attribute . '/' . $id, $category);
            Assert::assertStatusCode200($response->statusCode());

            // 21文字は登録できない
            $category = $this->validAttributeCategory();
            $category['name'] = 'Aput_abAB_efgt_a' . str_pad((string)$this->suffix, 5, '0', STR_PAD_LEFT);
            $response = $this->request->put('/attribute_categories/' . $attribute . '/' . $id, $category);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E105', $response->jsonBody()['code'], $attribute . ' nameが長い');
        }
    }

    /**
     * 属性カテゴリー更新バリデーションエラーテスト（名前が小文字から始まる）
     */
    public function testAttributeCategoryPutNameLowercase(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_categories/' . $attribute, $this->validAttributeCategory());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $category = $this->validAttributeCategory();
            $category['name'] = 'aaa';

            $response = $this->request->put('/attribute_categories/' . $attribute . '/' . $id, $category);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E103', $response->jsonBody()['code'], $attribute . ' nameが小文字始まり');
        }
    }

    /**
     * 属性カテゴリー更新バリデーションエラーテスト（名前がマルチバイト文字）
     */
    public function testAttributeCategoryPutNameMultibyte(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_categories/' . $attribute, $this->validAttributeCategory());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $category = $this->validAttributeCategory();
            $category['name'] = 'あああ';

            $response = $this->request->put('/attribute_categories/' . $attribute . '/' . $id, $category);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E103', $response->jsonBody()['code'], $attribute . ' nameがマルチバイト');
        }
    }

    /**
     * 属性カテゴリー更新バリデーションエラーテスト（説明が空）
     */
    public function testAttributeCategoryPutDescriptionEmpty(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_categories/' . $attribute, $this->validAttributeCategory());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $category = $this->validAttributeCategory();
            $category['description'] = '';

            $response = $this->request->put('/attribute_categories/' . $attribute . '/' . $id, $category);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' descriptionが空');
        }
    }

    /**
     * 属性カテゴリー更新バリデーションエラーテスト（説明がない）
     */
    public function testAttributeCategoryPutDescriptionMissing(): void
    {
        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $response = $this->request->post('/attribute_categories/' . $attribute, $this->validAttributeCategory());
            Assert::assertStatusCode200($response->statusCode());
            $id = $response->jsonBody()['id'];

            $category = $this->validAttributeCategory();
            unset($category['description']);

            $response = $this->request->put('/attribute_categories/' . $attribute . '/' . $id, $category);
            Assert::assertStatusCode400($response->statusCode());
            Assert::assertSame('E109', $response->jsonBody()['code'], $attribute . ' descriptionがない');
        }
    }

    /**
     * 認証なし
     */
    public function testAttributeCategoryWithoutAuth(): void
    {
        $noSessionRequest = new Request();

        $attributes = ['kind_category', 'purpose_category', 'place_category'];

        foreach ($attributes as $attribute) {
            $response = $noSessionRequest->get('/attribute_categories/' . $attribute);
            Assert::assertStatusCode401($response->statusCode());
            Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し一覧取得 ' . $attribute);

            $response = $noSessionRequest->post('/attribute_categories/' . $attribute, $this->validAttributeCategory());
            Assert::assertStatusCode401($response->statusCode());
            Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し登録 ' . $attribute);

            $response = $noSessionRequest->put('/attribute_categories/' . $attribute . '/10', $this->validAttributeCategory());
            Assert::assertStatusCode401($response->statusCode());
            Assert::assertSame('E201', $response->jsonBody()['code'], '認証無し更新 ' . $attribute);
        }
    }

    private function assertAttributeCategoryCRUD(string $attribute): void
    {
        // 登録
        $response = $this->request->post('/attribute_categories/' . $attribute, [
            'name' => 'Hoge' . $this->suffix,
            'description' => '説明',
        ]);
        Assert::assertStatusCode200($response->statusCode());
        $category = $response->jsonBody();
        Assert::assertSame('Hoge' . $this->suffix, $category['name'], $attribute . ' 登録後の name');
        Assert::assertSame('説明', $category['description'], $attribute . ' 登録後の description');

        $id = $category['id'];

        // 個別取得
        $response = $this->request->get('/attribute_categories/' . $attribute . '/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $category = $response->jsonBody();
        Assert::assertSame('Hoge' . $this->suffix, $category['name'], $attribute . ' 取得後の name');
        Assert::assertSame('説明', $category['description'], $attribute . ' 取得後の description');

        // 更新
        $response = $this->request->put('/attribute_categories/' . $attribute . '/' . $id, [
            'name' => 'Fuga' . $this->suffix,
            'description' => '説明更新後',
        ]);
        Assert::assertStatusCode200($response->statusCode());
        $category = $response->jsonBody();
        Assert::assertSame('Fuga' . $this->suffix, $category['name'], $attribute . ' 更新後の name');
        Assert::assertSame('説明更新後', $category['description'], $attribute . ' 更新後の description');

        // 更新後の値を確認
        $response = $this->request->get('/attribute_categories/' . $attribute . '/' . $id);
        Assert::assertStatusCode200($response->statusCode());
        $category = $response->jsonBody();
        Assert::assertSame('Fuga' . $this->suffix, $category['name'], $attribute . ' 更新確認後の name');
        Assert::assertSame('説明更新後', $category['description'], $attribute . ' 更新確認後の description');
    }

    private function validAttributeCategory(): array
    {
        $this->suffix += 1;

        return [
            'name' => 'Test' . $this->suffix,
            'description' => '説明',
        ];
    }
}
