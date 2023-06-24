<?php

namespace App\Models\Dao\Impl;

use Illuminate\Support\Facades\DB;
use App\Models\Dao\AttributeElementDao;

/**
 * attribute element を管理するための DAO
 */
class AttributeElementDaoImpl implements AttributeElementDao
{

    public function getAttributeElement(string $attributeName)
    {
        return DB::table(self::getAttributeTableName($attributeName))->get()->toArray();
    }

    public function getAttributeElementByElementId(string $attributeName, string $elementId)
    {
        return DB::table(self::getAttributeTableName($attributeName))->where('id', $elementId)->get()->toArray();
    }

    public function insertAttributeElement(string $attributeName, array $attributeElement)
    {
        $data = [
            'name' => $attributeElement['name'],
            'description' => $attributeElement['description'],
            'category_id' => $attributeElement['categoryId'],
        ];
        return DB::table(self::getAttributeTableName($attributeName))->insert($data);
    }

    public function updateAttributeElement(string $attributeName, array $attributeElement)
    {
        $data = [
            'name' => $attributeElement['name'],
            'description' => $attributeElement['description'],
            'category_id' => $attributeElement['categoryId'],
        ];
        return DB::table(self::getAttributeTableName($attributeName))->where('id', $attributeElement['id'])->update($data);
    }

    private static function getAttributeTableName(string $attributeName)
    {
        return match ($attributeName) {
            'kind_element' => 'm_kind_element',
            'purpose_element' => 'm_purpose_element',
            'place_element' => 'm_place_element',
            default => throw new Exception("No attribute {$attributeName}."),
        };
    }
}
