<?php

namespace App\Models\Dao;

use Illuminate\Support\Facades\DB;

/**
 * attribute category を管理するための DAO
 */
class AttributeCategoryDao
{

    public static function getAttributeCategories(string $attributeName)
    {
        return DB::table(self::getAttributeTableName($attributeName))->get()->toArray();
    }

    public static function insertAttributeCategory(string $attributeName, array $attributeCategory)
    {
        $data = [
            'name' => $attributeCategory['name'],
            'description' => $attributeCategory['description'],
        ];
        return DB::table(self::getAttributeTableName($attributeName))->insert($data);
    }

    public static function updateAttributeCategory(string $attributeName, array $attributeCategory)
    {
        $data = [
            'name' => $attributeCategory['name'],
            'description' => $attributeCategory['description'],
        ];
        return DB::table(self::getAttributeTableName($attributeName))->where('id', $attributeCategory['id'])->update($data);
    }

    private static function getAttributeTableName(string $attributeName)
    {
        return match ($attributeName) {
            'kind_category' => 'm_kind_category',
            'purpose_category' => 'm_purpose_category',
            'place_category' => 'm_place_category',
            default => throw new Exception("No attribute {$attributeName}."),
        };
    }
}
