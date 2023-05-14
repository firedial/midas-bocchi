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
            'name' => 'kind_move_none',
            'description' => 'kind_e_move_none',
            'category_id' => 1,
        ]);
        PurposeElement::create([
            'name' => 'purpose_move_none',
            'description' => 'purpose_e_move_none',
            'category_id' => 1,
        ]);
        PlaceElement::create([
            'name' => 'place_e_move_none',
            'description' => 'place_e_move_none',
            'category_id' => 1,
        ]);

        for ($i = 2; $i <= 100; $i++) {
            KindElement::create([
                'name' => 'kind_e_name' . $i,
                'description' => 'kind_e_desc' . $i,
                'category_id' => $i,
            ]);
            PurposeElement::create([
                'name' => 'purpose_e_name' . $i,
                'description' => 'purpose_e_desc' . $i,
                'category_id' => $i,
            ]);
            PlaceElement::create([
                'name' => 'place_e_name' . $i,
                'description' => 'place_e_desc' . $i,
                'category_id' => $i,
            ]);
        }
    }
}
