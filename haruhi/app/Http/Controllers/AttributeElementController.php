<?php

namespace App\Http\Controllers;

use Illuminate\Support\Facades\DB;
use Illuminate\Http\Request;
use App\Models\Balance;
use App\Models\KindElement;

class AttributeElementController extends Controller
{
    public function index(String $attributeName)
    {
        $tableName = '';
        if ($attributeName === 'kind_element') {
            $tableName = 'm_kind_element';
        } else if ($attributeName === 'purpose_element') {
            $tableName = 'm_purpose_element';
        } else if ($attributeName === 'place_element') {
            $tableName = 'm_place_element';
        } else {
            return '';
        }

        return DB::table($tableName . ' as element')
            ->select(
                'element.id',
                'element.name',
                'element.description',
                'element.category_id'
            )
            ->get();
    }
}
