<?php

namespace Database\Seeders;

use App\Models\Balance;
use Illuminate\Database\Seeder;

class BalanceTableSeeder extends Seeder
{
    /**
     * Run the database seeds.
     *
     * @return void
     */
    public function run()
    {
        for ($i = 2; $i <= 10; $i++) {
            Balance::create([
                'amount' => $i * $i,
                'item' => 'item' . $i,
                'kind_element_id' => $i,
                'purpose_element_id' => $i,
                'place_element_id' => $i,
                'date' => '2021-08-' . (10 + $i),
            ]);
        }
        for ($i = 2; $i <= 10; $i++) {
            Balance::create([
                'amount' => (-1) * $i * $i * $i,
                'item' => 'move' . $i,
                'kind_element_id' => 1,
                'purpose_element_id' => 1,
                'place_element_id' => $i,
                'date' => '2021-08-' . (10 + $i),
            ]);
            Balance::create([
                'amount' => $i * $i * $i,
                'item' => 'move' . $i,
                'kind_element_id' => 1,
                'purpose_element_id' => 1,
                'place_element_id' => $i + 1,
                'date' => '2021-08-' . (10 + $i),
            ]);

            Balance::create([
                'amount' => (-1) * $i * $i * $i,
                'item' => 'move' . $i,
                'kind_element_id' => 1,
                'purpose_element_id' => $i,
                'place_element_id' => 1,
                'date' => '2021-08-' . (10 + $i),
            ]);
            Balance::create([
                'amount' => $i * $i * $i,
                'item' => 'move' . $i,
                'kind_element_id' => 1,
                'purpose_element_id' => $i + 1,
                'place_element_id' => 1,
                'date' => '2021-08-' . (10 + $i),
            ]);
        }
    }
}
