<?php

use App\Models\Dao\Impl\BalanceDaoImpl;
use App\Service;
use Mockery\MockInterface;
use App\Models\User;
use function Pest\Laravel\{actingAs};
use Illuminate\Foundation\Testing\RefreshDatabase;

uses(RefreshDatabase::class);

test('移動表取得', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->get("/api/moves/purposes/");
    $response->assertStatus(200);
    // @todo 最大20件であることのテストも必要
    expect($response->json())->toHaveCount(9);

    // 降順取得なので決め打ちで大丈夫
    expect($response->json()[0])
        ->id->toBe(234)
        ->amount->toBe(1000)
        ->item->toBe("move10")
        ->before_id->toBe(10)
        ->after_id->toBe(11)
        ->date->toBe("2021-08-20")
        ->before_description->toBe("purpose_e_desc10")
        ->after_description->toBe("purpose_e_desc11");

    $response = $this->actingAs($user)->get("/api/moves/places/");
    $response->assertStatus(200);
    // @todo 最大20件であることのテストも必要
    expect($response->json())->toHaveCount(9);

    // 降順取得なので決め打ちで大丈夫
    expect($response->json()[0])
        ->id->toBe(232)
        ->amount->toBe(1000)
        ->item->toBe("move10")
        ->before_id->toBe(10)
        ->after_id->toBe(11)
        ->date->toBe("2021-08-20")
        ->before_description->toBe("place_e_desc10")
        ->after_description->toBe("place_e_desc11");
});

test('収支表取得(属性名不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->get("/api/moves/aaa/");
    // @todo 400 にする
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();
});

test('移動登録', function () {
    $this->seed();
    $user = User::factory()->create();

    // 登録
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    $response = $this->actingAs($user)->get("/api/moves/purposes/");
    expect($response->json()[0])
        ->amount->toBe(500)
        ->item->toBe("移動")
        ->before_id->toBe(2)
        ->after_id->toBe(3)
        ->date->toBe("2024-10-23")
        ->before_description->toBe("purpose_e_desc2")
        ->after_description->toBe("purpose_e_desc3");

    // 登録
    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    $response = $this->actingAs($user)->get("/api/moves/places/");
    expect($response->json()[0])
        ->amount->toBe(500)
        ->item->toBe("移動")
        ->before_id->toBe(2)
        ->after_id->toBe(3)
        ->date->toBe("2024-10-23")
        ->before_description->toBe("place_e_desc2")
        ->after_description->toBe("place_e_desc3");

    // うるう年の考慮
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-02-29",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-02-29",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト
});

test('移動登録(金額不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 金額が負
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => -500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => -500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 金額が0
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 0,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 0,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 金額がない
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 金額が文字
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => "aaa",
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => "aaa",
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('移動登録(項目不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 項目が空文字列
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "item" => "",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "item" => "",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 項目がない
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});


test('移動登録(要素不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 要素が移動id(=1)
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 1,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 1,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 1,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 1,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    // 外部キー不正
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 10000,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 10000,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 10000,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 10000,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    // パラメータなし
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "item" => "移動",
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "item" => "移動",
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    // パラメータが文字列
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => "aaa",
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => "aaa",
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => "aaa",
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => "aaa",
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    // 移動前後で同じの場合
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 2,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 2,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('移動登録(日付不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 日付なし
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 存在しない日付
    // 日付なし
    $response = $this->actingAs($user)->post(
        "/api/moves/purposes/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-06-31",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/moves/places/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-06-31",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('移動取得', function () {
    $this->seed();
    $user = User::factory()->create();

    // 取得
    $response = $this->actingAs($user)->get("/api/moves/purposes/234/");
    $response->assertStatus(200);
    expect($response->json())
        ->id->toBe(234)
        ->amount->toBe(1000)
        ->item->toBe("move10")
        ->before_id->toBe(10)
        ->after_id->toBe(11)
        ->date->toBe("2021-08-20")
        ->before_description->toBe("purpose_e_desc10")
        ->after_description->toBe("purpose_e_desc11");

    $response = $this->actingAs($user)->get("/api/moves/places/232/");
    $response->assertStatus(200);
    expect($response->json())
        ->id->toBe(232)
        ->amount->toBe(1000)
        ->item->toBe("move10")
        ->before_id->toBe(10)
        ->after_id->toBe(11)
        ->date->toBe("2021-08-20")
        ->before_description->toBe("place_e_desc10")
        ->after_description->toBe("place_e_desc11");
});

test('移動取得(存在しない)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 属性名が存在しない
    $response = $this->actingAs($user)->get("/api/moves/hoge/232/");
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    // そもそもレコードにない
    $response = $this->actingAs($user)->get("/api/moves/purposes/10000/");
    // @todo 404 に変える
    $response->assertStatus(500);

    $response = $this->actingAs($user)->get("/api/moves/places/10000/");
    // @todo 404 に変える
    $response->assertStatus(500);

    // 収支レコード
    $response = $this->actingAs($user)->get("/api/moves/purposes/10/");
    // @todo 404 に変える
    $response->assertStatus(500);

    $response = $this->actingAs($user)->get("/api/moves/places/10/");
    // @todo 404 に変える
    $response->assertStatus(500);

    // 前後のID
    $response = $this->actingAs($user)->get("/api/moves/purposes/233/");
    // @todo 404 に変える
    $response->assertStatus(500);
    $response = $this->actingAs($user)->get("/api/moves/purposes/235/");
    // @todo 404 に変える
    $response->assertStatus(500);

    $response = $this->actingAs($user)->get("/api/moves/places/231/");
    // @todo 404 に変える
    $response->assertStatus(500);
    $response = $this->actingAs($user)->get("/api/moves/places/233/");
    // @todo 404 に変える
    $response->assertStatus(500);

    // 別属性の移動
    $response = $this->actingAs($user)->get("/api/moves/purposes/232/");
    // @todo 404 に変える
    $response->assertStatus(200);

    $response = $this->actingAs($user)->get("/api/moves/places/234/");
    // @todo 404 に変える
    $response->assertStatus(200);
});


test('移動更新', function () {
    $this->seed();
    $user = User::factory()->create();

    // 更新
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 更新したデータの確認
    $response = $this->actingAs($user)->get("/api/moves/purposes/234/");
    expect($response->json())
        ->amount->toBe(500)
        ->item->toBe("移動")
        ->before_id->toBe(2)
        ->after_id->toBe(3)
        ->date->toBe("2024-10-23")
        ->before_description->toBe("purpose_e_desc2")
        ->after_description->toBe("purpose_e_desc3");

    // 更新
    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 登録したデータの確認
    $response = $this->actingAs($user)->get("/api/moves/places/232/");
    expect($response->json())
        ->amount->toBe(500)
        ->item->toBe("移動")
        ->before_id->toBe(2)
        ->after_id->toBe(3)
        ->date->toBe("2024-10-23")
        ->before_description->toBe("place_e_desc2")
        ->after_description->toBe("place_e_desc3");

    // うるう年の考慮
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-02-29",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-02-29",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト
});

test('移動更新(金額不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 金額が負
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => -500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => -500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 金額が0
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 0,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 0,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 金額がない
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 金額が文字
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => "aaa",
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => "aaa",
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('移動更新(項目不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 項目が空文字列
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "item" => "",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "item" => "",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 項目がない
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});


test('移動更新(要素不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 要素が移動id(=1)
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 1,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 1,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 1,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 1,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    // 外部キー不正
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 10000,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 10000,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 10000,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 10000,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    // パラメータなし
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "item" => "移動",
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    // パラメータが文字列
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => "aaa",
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => "aaa",
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => "aaa",
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => "aaa",
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    // 移動前後で同じの場合
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 2,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 2,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('移動更新(日付不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 日付なし
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 存在しない日付
    // 日付なし
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-06-31",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/232/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-06-31",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('移動更新(存在しない)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 属性名が存在しない
    $response = $this->actingAs($user)->put(
        "/api/moves/aaa/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    // そもそもレコードにない
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/10000/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 404 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/10000/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 404 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    // 収支レコード
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/10/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 404 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/10/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 404 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    // 前後のID
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/233/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 404 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/235/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 404 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/231/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 404 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/233/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 404 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    // 別属性の移動
    $response = $this->actingAs($user)->put(
        "/api/moves/purposes/232/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 404 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->put(
        "/api/moves/places/234/",
        [
            "amount" => 500,
            "item" => "移動",
            "before_id" => 2,
            "after_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 404 に変える
    $response->assertStatus(200);
    // expect($response->json())->message->toBeString();
});

test('移動削除', function () {
    $this->seed();
    $user = User::factory()->create();

    // 削除
    $response = $this->actingAs($user)->delete("/api/moves/purposes/234/");
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 削除したデータの確認
    $response = $this->actingAs($user)->get("/api/moves/purposes/234/");
    // @todo 404 にする
    $response->assertStatus(500);

    $response = $this->actingAs($user)->delete("/api/moves/places/232/");
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // 削除したデータの確認
    $response = $this->actingAs($user)->get("/api/moves/places/232/");
    // @todo 404 にする
    $response->assertStatus(500);
});

test('移動削除(存在しない)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 属性名が存在しない
    $response = $this->actingAs($user)->delete("/api/moves/aaa/232/");
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    // そもそもない
    $response = $this->actingAs($user)->delete("/api/moves/purposes/10000/");
    // @todo 404 に変える
    $response->assertStatus(200);

    $response = $this->actingAs($user)->delete("/api/moves/places/10000/");
    // @todo 404 に変える
    $response->assertStatus(200);

    // 収支レコード
    $response = $this->actingAs($user)->delete("/api/moves/purposes/10/");
    // @todo 404 に変える
    $response->assertStatus(200);

    $response = $this->actingAs($user)->delete("/api/moves/places/10/");
    // @todo 404 に変える
    $response->assertStatus(200);

    // 前後のID
    $response = $this->actingAs($user)->delete("/api/moves/purposes/233/");
    // @todo 404 に変える
    $response->assertStatus(200);
    $response = $this->actingAs($user)->delete("/api/moves/purposes/235/");
    // @todo 404 に変える
    $response->assertStatus(200);

    $response = $this->actingAs($user)->delete("/api/moves/places/231/");
    // @todo 404 に変える
    $response->assertStatus(200);
    $response = $this->actingAs($user)->delete("/api/moves/places/233/");
    // @todo 404 に変える
    $response->assertStatus(200);

    // 別属性の移動
    $response = $this->actingAs($user)->delete("/api/moves/purposes/232/");
    // @todo 404 に変える
    $response->assertStatus(200);

    $response = $this->actingAs($user)->delete("/api/moves/places/234/");
    // @todo 404 に変える
    $response->assertStatus(200);
});
