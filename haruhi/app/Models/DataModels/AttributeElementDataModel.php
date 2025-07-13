<?php

namespace App\Models\DataModels;

use Illuminate\Support\Facades\DB;

abstract class AttributeElementDataModel
{
    public const TABLE_NAME = "";

    public const C_ID = "";
    public const C_NAME = "";
    public const C_DESCRIPTION = "";
    public const C_PRIORITY = "";
    public const C_CATEGORY_ID = "";

    public static function selectAttributeElement(
        ?int $id = null,
    ): array {
        $query = DB::table(static::TABLE_NAME)
            ->select(
                static::C_ID,
                static::C_NAME,
                static::C_DESCRIPTION,
                static::C_PRIORITY,
                static::C_CATEGORY_ID,
            )
            ->orderby(static::C_PRIORITY, 'desc')
            ->orderby(static::C_ID, 'asc');
        if (!is_null($id)) {
            $query->where(static::C_ID, '=', $id);
        }

        return $query->get()->toArray();
    }

    public static function insertAttributeElement(
        string $name,
        string $description,
        int $priority,
        int $categoryId,
    ): int {
        return DB::table(static::TABLE_NAME)
            ->insertGetId([
                static::C_NAME => $name,
                static::C_DESCRIPTION => $description,
                static::C_PRIORITY => $priority,
                static::C_CATEGORY_ID => $categoryId,
            ]);
    }

    public static function updateAttributeElement(
        int $id,
        string $name,
        string $description,
        int $priority,
        int $categoryId,
    ): void {
        DB::table(static::TABLE_NAME)
            ->where(static::C_ID, '=', $id)
            ->update([
                static::C_NAME => $name,
                static::C_DESCRIPTION => $description,
                static::C_PRIORITY => $priority,
                static::C_CATEGORY_ID => $categoryId,
            ]);
    }
}
