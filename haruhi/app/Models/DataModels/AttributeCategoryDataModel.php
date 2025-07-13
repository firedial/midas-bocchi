<?php

namespace App\Models\DataModels;

use Illuminate\Support\Facades\DB;

abstract class AttributeCategoryDataModel
{
    public const TABLE_NAME = "";

    public const C_ID = "";
    public const C_NAME = "";
    public const C_DESCRIPTION = "";

    public static function selectAttributeCategory(
        ?int $id = null,
    ): array {
        $query = DB::table(static::TABLE_NAME)
            ->select(
                static::C_ID,
                static::C_NAME,
                static::C_DESCRIPTION,
            )
            ->orderby(static::C_ID, 'asc');
        if (!is_null($id)) {
            $query->where(static::C_ID, '=', $id);
        }

        return $query->get()->toArray();
    }

    public static function insertAttributeCategory(
        string $name,
        string $description,
    ): int {
        return DB::table(static::TABLE_NAME)
            ->insertGetId([
                static::C_NAME => $name,
                static::C_DESCRIPTION => $description,
            ]);
    }

    public static function updateAttributeCategory(
        int $id,
        string $name,
        string $description,
    ): void {
        DB::table(static::TABLE_NAME)
            ->where(static::C_ID, '=', $id)
            ->update([
                static::C_NAME => $name,
                static::C_DESCRIPTION => $description,
            ]);
    }
}
