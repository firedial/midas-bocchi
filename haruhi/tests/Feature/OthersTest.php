<?php

use App\Models\Dao\Impl\BalanceDaoImpl;
use App\Service;
use Mockery\MockInterface;
use App\Models\User;
use function Pest\Laravel\{actingAs};
use Illuminate\Foundation\Testing\RefreshDatabase;

uses(RefreshDatabase::class);

test('給料明細登録', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 0,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 0,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 0,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 0,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 0,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 0,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 0,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 0,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 0,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 0,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-29",
        ]
    );
    $response->assertStatus(200);
});

test('給料明細登録(異常系)', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => -1,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => -1,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => -1,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => -1,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => -1,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => -1,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => -1,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => -1,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => -1,
            "holding" => 15000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => -1,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/salary/",
        [
            "baseSalary" => 200000,
            "adjustmentSalary" => 50000,
            "transportation" => 6000,
            "holdingIncentives" => 1000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "residentTax" => 9000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "holding" => 15000,
            "date" => "2024-06-31",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('賞与明細登録', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->post(
        "/api/bonus/",
        [
            "bonus" => 200000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/bonus/",
        [
            "bonus" => 0,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/bonus/",
        [
            "bonus" => 200000,
            "healthInsurance" => 0,
            "welfarePension" => 3000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/bonus/",
        [
            "bonus" => 200000,
            "healthInsurance" => 12000,
            "welfarePension" => 0,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/bonus/",
        [
            "bonus" => 200000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "employmentInsurance" => 0,
            "incomeTax" => 14000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/bonus/",
        [
            "bonus" => 200000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "employmentInsurance" => 900,
            "incomeTax" => 0,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(200);

    $response = $this->actingAs($user)->post(
        "/api/bonus/",
        [
            "bonus" => 200000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "date" => "2024-02-29",
        ]
    );
    $response->assertStatus(200);
});

test('賞与明細登録(異常系)', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->post(
        "/api/bonus/",
        [
            "bonus" => -1,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/bonus/",
        [
            "bonus" => 200000,
            "healthInsurance" => -1,
            "welfarePension" => 3000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/bonus/",
        [
            "bonus" => 200000,
            "healthInsurance" => 12000,
            "welfarePension" => -1,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/bonus/",
        [
            "bonus" => 200000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "employmentInsurance" => -1,
            "incomeTax" => 14000,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/bonus/",
        [
            "bonus" => 200000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "employmentInsurance" => 900,
            "incomeTax" => -1,
            "date" => "2024-02-25",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/bonus/",
        [
            "bonus" => 200000,
            "healthInsurance" => 12000,
            "welfarePension" => 3000,
            "employmentInsurance" => 900,
            "incomeTax" => 14000,
            "date" => "2024-06-31",
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

test('月々の支払い登録', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->post(
        "/api/monthly/",
        [
            "house_rent" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "gas" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "water" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "elect" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "net" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
        ]
    );
    $response->assertStatus(200);
});

test('月々の支払い登録(異常系)', function () {
    $this->seed();
    $user = User::factory()->create();

    $response = $this->actingAs($user)->post(
        "/api/monthly/",
        [
            "house_rent" => [
                "amount" => -1,
                "date" => "2024-02-27",
            ],
            "gas" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "water" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "elect" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "net" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/monthly/",
        [
            "house_rent" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "gas" => [
                "amount" => -1,
                "date" => "2024-02-27",
            ],
            "water" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "elect" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "net" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/monthly/",
        [
            "house_rent" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "gas" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "water" => [
                "amount" => -1,
                "date" => "2024-02-27",
            ],
            "elect" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "net" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/monthly/",
        [
            "house_rent" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "gas" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "water" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "elect" => [
                "amount" => -1,
                "date" => "2024-02-27",
            ],
            "net" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();

    $response = $this->actingAs($user)->post(
        "/api/monthly/",
        [
            "house_rent" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "gas" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "water" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "elect" => [
                "amount" => 50000,
                "date" => "2024-02-27",
            ],
            "net" => [
                "amount" => -1,
                "date" => "2024-02-27",
            ],
        ]
    );
    $response->assertStatus(400);
    expect($response->json())->message->toBeString();
});

// @todo s_secret を入れる必要がある
// test('会社交通費登録', function () {
//     $this->seed();
//     $user = User::factory()->create();
//
//     $response = $this->actingAs($user)->post(
//         "/api/transportation/",
//         [
//             "date" => "2024-02-25",
//         ]
//     );
//     $response->assertStatus(200);
// });

// @todo 金額チェック登録
// @todo 秘匿情報取得 & 更新
