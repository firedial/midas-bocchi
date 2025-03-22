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
