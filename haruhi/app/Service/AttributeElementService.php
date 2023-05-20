<?php

namespace App\Service;

use App\Models\KindElement;
use App\Models\PurposeElement;
use App\Models\PlaceElement;
use App\Models\Balance;
use App\Util\Date;
use App\Exceptions\InvalidParameterException;
use App\Models\Dao\AttributeElementDao;
use Illuminate\Support\Facades\DB;

/**
 *  attributeEelement テーブルの操作のサービスクラス
 */
class AttributeElementService
{
    public static function getAttributeElements(array $input): array
    {
        $records = AttributeElementDao::getAttributeElement($input['attributeName']);
        return array_map(
            function ($record) {
                return [
                    'id' => $record->id,
                    'name' => $record->name,
                    'description' => $record->description,
                    'category_id' => $record->category_id,
                ];
            },
            $records
        );
    }

    public static function getAttributeElementByElementId(array $input): array
    {
        $records = AttributeElementDao::getAttributeElementByElementId($input['attributeName'], $input['elementId']);
        return array_map(
            function ($record) {
                return [
                    'id' => $record->id,
                    'name' => $record->name,
                    'description' => $record->description,
                    'category_id' => $record->category_id,
                ];
            },
            $records
        );
    }

    public static function createAttributeElement(array $input): Bool
    {
        return AttributeElementDao::insertAttributeElement($input['attributeName'], $input['attributeElement']);
    }

    public static function updateAttributeElement(array $input): Bool
    {
        return AttributeElementDao::updateAttributeElement($input['attributeName'], $input['attributeElement']);
    }
}
