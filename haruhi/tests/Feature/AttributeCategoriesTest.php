<?php

use App\Models\Dao\Impl\BalanceDaoImpl;
use App\Service;
use Mockery\MockInterface;
use App\Models\User;
use function Pest\Laravel\{actingAs};
use Illuminate\Foundation\Testing\RefreshDatabase;

uses(RefreshDatabase::class);

test('属性カテゴリー表取得', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->get("/api/attribute_categories/kind_category/");
    $response->assertStatus(200);
    expect($response->json())->toHaveCount(100);

    expect($response->json()[0])
        ->id->toBeInt()
        ->name->toBeString()
        ->description->toBeString();

    $response = $this->actingAs($user)->get("/api/attribute_categories/purpose_category/");
    $response->assertStatus(200);
    expect($response->json())->toHaveCount(100);

    expect($response->json()[0])
        ->id->toBeInt()
        ->name->toBeString()
        ->description->toBeString();

    $response = $this->actingAs($user)->get("/api/attribute_categories/place_category/");
    $response->assertStatus(200);
    expect($response->json())->toHaveCount(100);

    expect($response->json()[0])
        ->id->toBeInt()
        ->name->toBeString()
        ->description->toBeString();
});

test('属性カテゴリー表取得(属性名不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->get("/api/attribute_categories/aaa/");
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('属性カテゴリー登録', function () {
    $this->seed();
    $user = User::factory()->create();

    // 登録
    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/kind_category/",
        [
            "name" => "Hoge",
            "description" => "説明",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    // @todo id で取得するようにする
    // $response = $this->actingAs($user)->get("/api/attribute_categories/category_element/");
    // expect($response->json()[0])
    //     ->name->toBe("hoge")
    //     ->description->toBe("説明");

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/purpose_category/",
        [
            "name" => "Hoge",
            "description" => "説明",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    // @todo id で取得するようにする
    // $response = $this->actingAs($user)->get("/api/attribute_categories/category_element/");
    // expect($response->json()[0])
    //     ->name->toBe("hoge")
    //     ->description->toBe("説明");

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/place_category/",
        [
            "name" => "Hoge",
            "description" => "説明",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    // @todo id で取得するようにする
    // $response = $this->actingAs($user)->get("/api/attribute_categories/category_element/");
    // expect($response->json()[0])
    //     ->name->toBe("hoge")
    //     ->description->toBe("説明");
});

test('属性カテゴリー登録(名前不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 名前が空
    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/kind_category/",
        [
            "name" => "",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/purpose_category/",
        [
            "name" => "",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/place_category/",
        [
            "name" => "",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 名前がなし
    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/kind_category/",
        [
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/purpose_category/",
        [
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/place_category/",
        [
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 名前が小文字から始まる
    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/kind_category/",
        [
            "name" => "aaa",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/purpose_category/",
        [
            "name" => "aaa",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/place_category/",
        [
            "name" => "aaa",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // マルチバイト文字が入っている
    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/kind_category/",
        [
            "name" => "あああ",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/purpose_category/",
        [
            "name" => "あああ",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/place_category/",
        [
            "name" => "あああ",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('属性カテゴリー登録(説明不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 説明が空文字列
    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/kind_category/",
        [
            "name" => "Aaa",
            "description" => "",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/purpose_category/",
        [
            "name" => "Aaa",
            "description" => "",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/place_category/",
        [
            "name" => "Aaa",
            "description" => "",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 説明がなし
    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/kind_category/",
        [
            "name" => "Aaa",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/purpose_category/",
        [
            "name" => "Aaa",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/place_category/",
        [
            "name" => "Aaa",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('属性カテゴリー更新', function () {
    $this->seed();
    $user = User::factory()->create();

    // 更新
    $response = $this->actingAs($user)->put(
        "/api/attribute_categories/kind_category/10/",
        [
            "name" => "Hoge",
            "description" => "説明",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    $response = $this->actingAs($user)->get("/api/attribute_categories/kind_category/");
    expect(array_values(array_filter(
        $response->json(),
        fn($x) => $x["id"] === 10,
    ))[0])
        ->name->toBe("Hoge")
        ->description->toBe("説明");

    $response = $this->actingAs($user)->put(
        "/api/attribute_categories/purpose_category/10/",
        [
            "name" => "Hoge",
            "description" => "説明",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    $response = $this->actingAs($user)->get("/api/attribute_categories/purpose_category/");
    expect(array_values(array_filter(
        $response->json(),
        fn($x) => $x["id"] === 10,
    ))[0])
        ->name->toBe("Hoge")
        ->description->toBe("説明");

    $response = $this->actingAs($user)->put(
        "/api/attribute_categories/place_category/10/",
        [
            "name" => "Hoge",
            "description" => "説明",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    $response = $this->actingAs($user)->get("/api/attribute_categories/place_category/");
    expect(array_values(array_filter(
        $response->json(),
        fn($x) => $x["id"] === 10,
    ))[0])
        ->name->toBe("Hoge")
        ->description->toBe("説明");
});

test('属性カテゴリー更新(名前不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 名前が空
    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/kind_category/",
        [
            "name" => "",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/purpose_category/",
        [
            "name" => "",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/place_category/",
        [
            "name" => "",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 名前がなし
    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/kind_category/",
        [
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/purpose_category/",
        [
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/place_category/",
        [
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 名前が小文字から始まる
    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/kind_category/",
        [
            "name" => "aaa",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/purpose_category/",
        [
            "name" => "aaa",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/place_category/",
        [
            "name" => "aaa",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // マルチバイト文字が入っている
    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/kind_category/",
        [
            "name" => "あああ",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/purpose_category/",
        [
            "name" => "あああ",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/place_category/",
        [
            "name" => "あああ",
            "description" => "説明",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('属性カテゴリー更新(説明不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 説明が空文字列
    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/kind_category/",
        [
            "name" => "Aaa",
            "description" => "",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/purpose_category/",
        [
            "name" => "Aaa",
            "description" => "",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/place_category/",
        [
            "name" => "Aaa",
            "description" => "",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 説明がなし
    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/kind_category/",
        [
            "name" => "Aaa",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/purpose_category/",
        [
            "name" => "Aaa",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_categories/place_category/",
        [
            "name" => "Aaa",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});
