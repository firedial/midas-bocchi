<?php

namespace App\Models\DataModels;

use Illuminate\Support\Facades\DB;

class SecretDataModel
{
    private const TABLE_NAME = "s_secret";

    private const C_ID = "id";
    private const C_VALUE = "value";

    public static function selectSecret(): array
    {
        $query = DB::table(self::TABLE_NAME)
            ->select(
                self::C_ID,
                self::C_VALUE,
            );

        return $query->get()->toArray();
    }

    public static function updateSecret(string $value): void
    {
        DB::table(self::TABLE_NAME)
            ->where(self::C_ID, '=', 1)
            ->update([
                self::C_VALUE => $value,
            ]);
    }
}
