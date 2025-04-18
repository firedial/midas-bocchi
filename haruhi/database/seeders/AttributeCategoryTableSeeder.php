<?php

namespace Database\Seeders;

use App\Models\KindCategory;
use App\Models\PurposeCategory;
use App\Models\PlaceCategory;
use Illuminate\Database\Seeder;

class AttributeCategoryTableSeeder extends Seeder
{
    /**
     * Run the database seeds.
     *
     * @return void
     */
    public function run()
    {
        KindCategory::create([
            'id' => 1,
            'name' => 'kind_c_move_none',
            'description' => 'kind_c_move_none',
        ]);
        PurposeCategory::create([
            'id' => 1,
            'name' => 'purpose_c_move_none',
            'description' => 'purpose_c_move_none',
        ]);
        PlaceCategory::create([
            'id' => 1,
            'name' => 'place_c_move_none',
            'description' => 'place_c_move_none',
        ]);

        for ($i = 2; $i <= 100; $i++) {
            KindCategory::create([
                'id' => $i,
                'name' => 'kind_c_name' . $i,
                'description' => 'kind_c_desc' . $i,
            ]);
            PurposeCategory::create([
                'id' => $i,
                'name' => 'purpose_c_name' . $i,
                'description' => 'purpose_c_desc' . $i,
            ]);
            PlaceCategory::create([
                'id' => $i,
                'name' => 'place_c_name' . $i,
                'description' => 'place_c_desc' . $i,
            ]);
        }
    }
}
