<?php

use App\Models\Dao\Impl\BalanceDaoImpl;
use App\Service;
use Mockery\MockInterface;

test('収支表取得', function () {
    $response = $this->get("/api/balances");
    // $response->assertStatus(200);
    // expect($response)->toHaveCount(0);
    expect($response->json())->message->toBeString();
});

use App\Models\User;
use function Pest\Laravel\{actingAs};

test('authenticated user can access the dashboard', function () {

    // $mock = $this->mock(BalanceService::class, function (MockInterface $mock) {
    //     $mock->shouldReceive('getBalances')->once();
    // });

    $this->mock(BalanceDaoImpl::class)
      ->shouldReceive('selectBalance')
      ->once()
      ->andReturn([]);

    $user = User::factory()->create();
    // $this->actingAs($user)->get("/api/balances")->assertStatus(200);
    expect($this->actingAs($user)->get("/api/balances")->json())->toBeString();
});
