<?php

use App\Models\Dao\Impl\BalanceDaoImpl;
use App\Service;
use Mockery\MockInterface;
use App\Models\User;
use function Pest\Laravel\{actingAs};
use Illuminate\Foundation\Testing\RefreshDatabase;

uses(RefreshDatabase::class);

test('収支表取得', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->get("/api/balances/");
    $response->assertStatus(200);
    expect($response->json())->toHaveCount(199);

    expect($response->json()[0])
        ->id->toBe(1)
        ->amount->toBe(4)
        ->item->toBe("item2")
        ->kind_element_id->toBe(2)
        ->purpose_element_id->toBe(2)
        ->place_element_id->toBe(2)
        ->date->toBe("2021-08-12")
        ->kind_element_description->toBe("kind_e_desc2")
        ->purpose_element_description->toBe("purpose_e_desc2")
        ->place_element_description->toBe("place_e_desc2");
});

test('収支表取得(件数指定)', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->get("/api/balances/?limit=20");
    $response->assertStatus(200);
    expect($response->json())->toHaveCount(20);
});

test('収支表取得(並び替え)', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->get("/api/balances/?orderby=desc");
    $response->assertStatus(200);
    expect($response->json()[0])->id->toBe(199);

    $response = $this->actingAs($user)->get("/api/balances/?orderby=asc");
    $response->assertStatus(200);
    expect($response->json()[0])->id->toBe(1);
});

test('収支表取得(件数指定と並び替え)', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->get("/api/balances/?orderby=desc&limit=20");
    $response->assertStatus(200);
    expect($response->json()[0])->id->toBe(199);
    expect($response->json())->toHaveCount(20);
});


test('収支表取得(件数指定パラメータ不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->get("/api/balances/?limit=aaa");
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // @todo
    // $response = $this->actingAs($user)->get("/api/balances/?limit=-1");
    // $response->assertStatus(400);
    // expect($response->json())->message->toBeString();

    // $response = $this->actingAs($user)->get("/api/balances/?limit=0");
    // $response->assertStatus(400);
    // expect($response->json())->message->toBeString();
});

test('収支表取得(並び替えパラメータ不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->get("/api/balances/?orderby=aaa");
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('収支登録', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => -500,
            "item" => "うどん",
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => 500,
            "item" => "収入",
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト

    // うるう年の考慮
    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => 500,
            "item" => "収入",
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "place_element_id" => 4,
            "date" => "2024-02-29",
        ]
    );
    $response->assertStatus(200);
    // @todo 返り値についてのテスト
});

test('収支登録(金額不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 金額が0
    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => 0,
            "item" => "収入",
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 金額がない
    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "item" => "収入",
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 金額が文字
    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => "aaa",
            "item" => "収入",
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('収支登録(項目不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 項目が空文字列
    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => 500,
            "item" => "",
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 項目がない
    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => 500,
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});


test('収支登録(要素不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 要素が移動id(=1)
    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => -500,
            "item" => "うどん",
            "kind_element_id" => 1,
            "purpose_element_id" => 3,
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => -500,
            "item" => "うどん",
            "kind_element_id" => 2,
            "purpose_element_id" => 1,
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => -500,
            "item" => "うどん",
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "place_element_id" => 1,
            "date" => "2024-10-23",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 外部キー不正
    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => -500,
            "item" => "うどん",
            "kind_element_id" => 10000,
            "purpose_element_id" => 3,
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => -500,
            "item" => "うどん",
            "kind_element_id" => 2,
            "purpose_element_id" => 10000,
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => -500,
            "item" => "うどん",
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "place_element_id" => 10000,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    // パラメータなし
    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => -500,
            "item" => "うどん",
            "purpose_element_id" => 3,
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => -500,
            "item" => "うどん",
            "kind_element_id" => 2,
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

        $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => -500,
            "item" => "うどん",
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    // パラメータが文字列
    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => -500,
            "item" => "うどん",
            "kind_element_id" => "aaa",
            "purpose_element_id" => 3,
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

        $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => -500,
            "item" => "うどん",
            "kind_element_id" => 2,
            "purpose_element_id" => "aaa",
            "place_element_id" => 4,
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => -500,
            "item" => "うどん",
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "place_element_id" => "aaa",
            "date" => "2024-10-23",
        ]
    );
    // @todo 400 に変える
    $response->assertStatus(500);
    expect($response->json())->message->toBeString();
});

test('収支登録(日付不正)', function () {
    $this->seed();
    $user = User::factory()->create();

    // 日付なし
    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => 500,
            "item" => "収入",
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "place_element_id" => 4,
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    // 存在しない日付
    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => 500,
            "item" => "収入",
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "place_element_id" => 4,
            "date" => "2025-06-31",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/balances/",
        [
            "amount" => 500,
            "item" => "収入",
            "kind_element_id" => 2,
            "purpose_element_id" => 3,
            "place_element_id" => 4,
            "date" => "2025-02-29",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});
