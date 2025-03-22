<?php

use App\Models\Dao\Impl\BalanceDaoImpl;
use App\Service;
use Mockery\MockInterface;
use App\Models\User;
use function Pest\Laravel\{actingAs};
use Illuminate\Foundation\Testing\RefreshDatabase;

uses(RefreshDatabase::class);

test('属性要素表取得', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->get("/api/attribute_elements/kind_element/");
    $response->assertStatus(200);
    expect($response->json())->toHaveCount(100);

    expect($response->json()[0])
        ->id->toBeInt()
        ->name->toBeString()
        ->description->toBeString()
        ->priority->toBeInt()
        ->category_id->toBeInt();

    $response = $this->actingAs($user)->get("/api/attribute_elements/purpose_element/");
    $response->assertStatus(200);
    expect($response->json())->toHaveCount(100);

    expect($response->json()[0])
        ->id->toBeInt()
        ->name->toBeString()
        ->description->toBeString()
        ->priority->toBeInt()
        ->category_id->toBeInt();

    $response = $this->actingAs($user)->get("/api/attribute_elements/place_element/");
    $response->assertStatus(200);
    expect($response->json())->toHaveCount(100);

    expect($response->json()[0])
        ->id->toBeInt()
        ->name->toBeString()
        ->description->toBeString()
        ->priority->toBeInt()
        ->category_id->toBeInt();

    // 優先度が0を省く
    $response = $this->actingAs($user)->get("/api/attribute_elements/kind_element/?isOnlySelectable=true");
    $response->assertStatus(200);
    expect($response->json())->toHaveCount(91);

    $response = $this->actingAs($user)->get("/api/attribute_elements/purpose_element/?isOnlySelectable=true");
    $response->assertStatus(200);
    expect($response->json())->toHaveCount(91);

    $response = $this->actingAs($user)->get("/api/attribute_elements/place_element/?isOnlySelectable=true");
    $response->assertStatus(200);
    expect($response->json())->toHaveCount(91);
});

test('属性要素表取得(属性名不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->get("/api/attribute_elements/aaa/");
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('属性要素登録', function () {
    $this->seed();
    $user = User::factory()->create();

    // 登録
    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/kind_element/",
        [
            "name" => "hoge",
            "description" => "説明",
            "priority" => 100,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    $response = $this->actingAs($user)->get("/api/attribute_elements/kind_element/");
    expect($response->json()[0])
        ->name->toBe("hoge")
        ->description->toBe("説明")
        ->priority->toBe(100)
        ->category_id->toBe(2);

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/purpose_element/",
        [
            "name" => "hoge",
            "description" => "説明",
            "priority" => 100,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    $response = $this->actingAs($user)->get("/api/attribute_elements/purpose_element/");
    expect($response->json()[0])
        ->name->toBe("hoge")
        ->description->toBe("説明")
        ->priority->toBe(100)
        ->category_id->toBe(2);

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/place_element/",
        [
            "name" => "hoge",
            "description" => "説明",
            "priority" => 100,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    $response = $this->actingAs($user)->get("/api/attribute_elements/place_element/");
    expect($response->json()[0])
        ->name->toBe("hoge")
        ->description->toBe("説明")
        ->priority->toBe(100)
        ->category_id->toBe(2);

    // 優先度が0
    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/kind_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 0,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/purpose_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 0,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/place_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 0,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(200);
});

test('属性要素登録(名前不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 名前が空
    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/kind_element/",
        [
            "name" => "",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/purpose_element/",
        [
            "name" => "",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/place_element/",
        [
            "name" => "",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 名前がなし
    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/kind_element/",
        [
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/purpose_element/",
        [
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/place_element/",
        [
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 名前が大文字から始まる
    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/kind_element/",
        [
            "name" => "Aaa",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/purpose_element/",
        [
            "name" => "Aaa",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/place_element/",
        [
            "name" => "Aaa",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // マルチバイト文字が入っている
    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/kind_element/",
        [
            "name" => "あああ",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/purpose_element/",
        [
            "name" => "あああ",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/place_element/",
        [
            "name" => "あああ",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('属性要素登録(説明不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 説明が空文字列
    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/kind_element/",
        [
            "name" => "aaa",
            "description" => "",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/purpose_element/",
        [
            "name" => "aaa",
            "description" => "",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/place_element/",
        [
            "name" => "aaa",
            "description" => "",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 説明がなし
    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/kind_element/",
        [
            "name" => "aaa",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/purpose_element/",
        [
            "name" => "aaa",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/place_element/",
        [
            "name" => "aaa",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});


test('属性要素登録(優先度不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 優先度が負
    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/kind_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => -1,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/purpose_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => -1,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/place_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => -1,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 優先度が101以上
    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/kind_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 101,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/purpose_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 101,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/place_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 101,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('属性要素登録(親カテゴリ不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 親カテゴリが移動ID(=1)
    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/kind_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 1,
        ]
    );
    // @todo 400 にする
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/purpose_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 1,
        ]
    );
    // @todo 400 にする
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/place_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 1,
        ]
    );
    // @todo 400 にする
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    // 親カテゴリなし
    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/kind_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 10,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/purpose_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 10,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/attribute_elements/place_element/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 10,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('属性要素取得', function () {
    $this->seed();
    $user = User::factory()->create();

    // 取得
    $response = $this->actingAs($user)->get("/api/attribute_elements/kind_element/20/");
    $response->assertStatus(200);
    expect($response->json())
        ->id->toBe(20)
        ->name->toBe("kind_e_name20")
        ->description->toBe("kind_e_desc20")
        ->priority->toBe(2)
        ->category_id->toBe(20);

    $response = $this->actingAs($user)->get("/api/attribute_elements/purpose_element/20/");
    $response->assertStatus(200);
    expect($response->json())
        ->id->toBe(20)
        ->name->toBe("purpose_e_name20")
        ->description->toBe("purpose_e_desc20")
        ->priority->toBe(2)
        ->category_id->toBe(20);

    $response = $this->actingAs($user)->get("/api/attribute_elements/place_element/20/");
    $response->assertStatus(200);
    expect($response->json())
        ->id->toBe(20)
        ->name->toBe("place_e_name20")
        ->description->toBe("place_e_desc20")
        ->priority->toBe(2)
        ->category_id->toBe(20);
});

test('属性要素取得(存在しない)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 属性名が存在しない
    $response = $this->actingAs($user)->get("/api/attribute_elements/aaa/20/");
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // そもそもレコードにない
    $response = $this->actingAs($user)->get("/api/attribute_elements/kind_element/10000/");
    // @todo 404 に変える
    $response->assertStatus(500);

    $response = $this->actingAs($user)->get("/api/attribute_elements/purpose_element/10000/");
    // @todo 404 に変える
    $response->assertStatus(500);

    $response = $this->actingAs($user)->get("/api/attribute_elements/place_element/10000/");
    // @todo 404 に変える
    $response->assertStatus(500);
});

test('属性要素更新', function () {
    $this->seed();
    $user = User::factory()->create();

    // 登録
    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/kind_element/10/",
        [
            "name" => "hoge",
            "description" => "説明",
            "priority" => 100,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    $response = $this->actingAs($user)->get("/api/attribute_elements/kind_element/10/");
    expect($response->json())
        ->name->toBe("hoge")
        ->description->toBe("説明")
        ->priority->toBe(100)
        ->category_id->toBe(2);

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/purpose_element/10/",
        [
            "name" => "hoge",
            "description" => "説明",
            "priority" => 100,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    $response = $this->actingAs($user)->get("/api/attribute_elements/purpose_element/10/");
    expect($response->json())
        ->name->toBe("hoge")
        ->description->toBe("説明")
        ->priority->toBe(100)
        ->category_id->toBe(2);

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/place_element/10/",
        [
            "name" => "hoge",
            "description" => "説明",
            "priority" => 100,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    $response = $this->actingAs($user)->get("/api/attribute_elements/place_element/10/");
    expect($response->json())
        ->name->toBe("hoge")
        ->description->toBe("説明")
        ->priority->toBe(100)
        ->category_id->toBe(2);
});

test('属性要素更新(名前不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 名前が空
    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/kind_element/10/",
        [
            "name" => "",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/purpose_element/10/",
        [
            "name" => "",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/place_element/10/",
        [
            "name" => "",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 名前がなし
    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/kind_element/10/",
        [
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/purpose_element/10/",
        [
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/place_element/10/",
        [
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 名前が大文字から始まる
    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/kind_element/10/",
        [
            "name" => "Aaa",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/purpose_element/10/",
        [
            "name" => "Aaa",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/place_element/10/",
        [
            "name" => "Aaa",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // マルチバイト文字が入っている
    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/kind_element/10/",
        [
            "name" => "あああ",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/purpose_element/10/",
        [
            "name" => "あああ",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/place_element/10/",
        [
            "name" => "あああ",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('属性要素更新(説明不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 説明が空文字列
    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/kind_element/10/",
        [
            "name" => "aaa",
            "description" => "",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/purpose_element/10/",
        [
            "name" => "aaa",
            "description" => "",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/place_element/10/",
        [
            "name" => "aaa",
            "description" => "",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 説明がなし
    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/kind_element/10/",
        [
            "name" => "aaa",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/purpose_element/10/",
        [
            "name" => "aaa",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/place_element/10/",
        [
            "name" => "aaa",
            "priority" => 10,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});


test('属性要素更新(優先度不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 優先度が負
    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/kind_element/10/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => -1,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/purpose_element/10/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => -1,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/place_element/10/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => -1,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 優先度が101以上
    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/kind_element/10/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 101,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/purpose_element/10/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 101,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/place_element/10/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 101,
            "category_id" => 2,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('属性要素更新(親カテゴリ不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 親カテゴリが移動ID(=1)
    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/kind_element/10/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 1,
        ]
    );
    // @todo 400 にする
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/purpose_element/10/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 1,
        ]
    );
    // @todo 400 にする
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/place_element/10/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 10,
            "category_id" => 1,
        ]
    );
    // @todo 400 にする
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    // 親カテゴリなし
    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/kind_element/10/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 10,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/purpose_element/10/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 10,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/attribute_elements/place_element/10/",
        [
            "name" => "aaa",
            "description" => "説明",
            "priority" => 10,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

