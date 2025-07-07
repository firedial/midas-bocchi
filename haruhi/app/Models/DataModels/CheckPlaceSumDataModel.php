<?php

namespace App\Models\DataModels;

use Illuminate\Support\Facades\DB;

class CheckPlaceSumDataModel
{
    private const TABLE_NAME = "r_check_place_sum";

    private const C_ID = "id";
    private const C_SUM = "sum";
    private const C_PLACE_ELEMENT_ID = "place_element_id";
    private const C_DATE = "date";

    public static function insert(
        int $sum,
        int $placeElementId,
        string $date,
    ): int {
        return DB::table(self::TABLE_NAME)
            ->insertGetId([
                self::C_SUM => $sum,
                self::C_PLACE_ELEMENT_ID => $placeElementId,
                self::C_DATE => $date,
            ]);
    }
}
