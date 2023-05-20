<?php

namespace App\Service;

use App\Models\KindElement;
use App\Models\PurposeElement;
use App\Models\PlaceElement;
use App\Models\Balance;
use App\Util\Date;
use App\Exceptions\InvalidParameterException;
use App\Models\Dao\AttributeCategoryDao;
use Illuminate\Support\Facades\DB;

/**
 *  attributeCategory テーブルの操作のサービスクラス
 */
class AttributeCategoryService
{
    public static function getAttributeCategories(array $input): array
    {
        $records = AttributeCategoryDao::getAttributeCategories($input['attributeName']);
        return array_map(
            function ($record) {
                return [
                    'id' => $record->id,
                    'name' => $record->name,
                    'description' => $record->description,
                ];
            },
            $records
        );
    }

    public static function createAttributeCategory(array $input): Bool
    {
        return AttributeCategoryDao::insertAttributeCategory($input['attributeName'], $input['attributeCategory']);
    }

    public static function updateAttributeCategory(array $input): Bool
    {
        return AttributeCategoryDao::updateAttributeCategory($input['attributeName'], $input['attributeCategory']);
    }
}
