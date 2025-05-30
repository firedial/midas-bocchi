<?php

namespace Database\Seeders;

use App\Models\KindElement;
use App\Models\PurposeElement;
use App\Models\PlaceElement;
use Illuminate\Database\Seeder;

class AttributeElementTableSeeder extends Seeder
{
    /**
     * Run the database seeds.
     *
     * @return void
     */
    public function run()
    {
        KindElement::create([
            'id' => 1,
            'name' => 'kind_move_none',
            'description' => 'kind_e_move_none',
            'priority' => 0,
            'category_id' => 1,
        ]);
        PurposeElement::create([
            'id' => 1,
            'name' => 'purpose_move_none',
            'description' => 'purpose_e_move_none',
            'priority' => 0,
            'category_id' => 1,
        ]);
        PlaceElement::create([
            'id' => 1,
            'name' => 'place_e_move_none',
            'description' => 'place_e_move_none',
            'priority' => 0,
            'category_id' => 1,
        ]);

        for ($i = 2; $i <= 100; $i++) {
            KindElement::create([
                'id' => $i,
                'name' => 'kind_e_name' . $i,
                'description' => 'kind_e_desc' . $i,
                'priority' => (int)($i / 10),
                'category_id' => $i,
            ]);
            PurposeElement::create([
                'id' => $i,
                'name' => 'purpose_e_name' . $i,
                'description' => 'purpose_e_desc' . $i,
                'priority' => (int)($i / 10),
                'category_id' => $i,
            ]);
            PlaceElement::create([
                'id' => $i,
                'name' => 'place_e_name' . $i,
                'description' => 'place_e_desc' . $i,
                'priority' => (int)($i / 10),
                'category_id' => $i,
            ]);
        }
    }
}
