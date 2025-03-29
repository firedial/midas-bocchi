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
                'id' => $i - 1,
                'amount' => $i * $i,
                'item' => 'item' . $i,
                'kind_element_id' => $i,
                'purpose_element_id' => $i,
                'place_element_id' => $i,
                'date' => '2021-08-' . (10 + $i),
            ]);
        }
        for ($i = 11; $i <= 200; $i++) {
            Balance::create([
                'id' => $i - 1,
                'amount' => (-1) * $i * $i,
                'item' => 'item' . $i,
                'kind_element_id' => ($i % 50) + 10,
                'purpose_element_id' => ($i % 50) + 10,
                'place_element_id' => ($i % 50) + 10,
                'date' => '2021-09-' . (($i % 20) + 1),
            ]);
        }

        // move
        for ($i = 2; $i <= 10; $i++) {
            Balance::create([
                'id' => 192 + $i * 4,
                'amount' => (-1) * $i * $i * $i,
                'item' => 'move' . $i,
                'kind_element_id' => 1,
                'purpose_element_id' => 1,
                'place_element_id' => $i,
                'date' => '2021-08-' . (10 + $i),
            ]);
            Balance::create([
                'id' => 193 + $i * 4,
                'amount' => $i * $i * $i,
                'item' => 'move' . $i,
                'kind_element_id' => 1,
                'purpose_element_id' => 1,
                'place_element_id' => $i + 1,
                'date' => '2021-08-' . (10 + $i),
            ]);

            Balance::create([
                'id' => 194 + $i * 4,
                'amount' => (-1) * $i * $i * $i,
                'item' => 'move' . $i,
                'kind_element_id' => 1,
                'purpose_element_id' => $i,
                'place_element_id' => 1,
                'date' => '2021-08-' . (10 + $i),
            ]);
            Balance::create([
                'id' => 195 + $i * 4,
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
