<?php

namespace App\Models\DataModels;

use Illuminate\Support\Facades\DB;

class ScenarioDetailDataModel
{
    private const TABLE_NAME = 'scenario_detail';

    private const C_SCENARIO_ID       = 'scenario_id';
    private const C_SEQ               = 'seq';
    private const C_TYPE              = 'type';
    private const C_AMOUNT            = 'amount';
    private const C_ITEM              = 'item';
    private const C_TYPE_ELEMENT_ID   = 'type_element_id';
    private const C_PURPOSE_ELEMENT_ID = 'purpose_element_id';
    private const C_PLACE_ELEMENT_ID  = 'place_element_id';
    private const C_MOVE_KIND         = 'move_kind';
    private const C_MOVE_BEFORE_ID    = 'move_before_id';
    private const C_MOVE_AFTER_ID     = 'move_after_id';

    public static function selectScenarioDetails(int $scenarioId): array
    {
        return DB::table(self::TABLE_NAME)
            ->where(self::C_SCENARIO_ID, '=', $scenarioId)
            ->orderBy(self::C_SEQ)
            ->get()
            ->toArray();
    }

    public static function insertScenarioDetails(array $rows): void
    {
        DB::table(self::TABLE_NAME)->insert($rows);
    }

    public static function deleteScenarioDetails(int $scenarioId): void
    {
        DB::table(self::TABLE_NAME)
            ->where(self::C_SCENARIO_ID, '=', $scenarioId)
            ->delete();
    }
}
