<?php

namespace App\Models\DataModels;

use Illuminate\Support\Facades\DB;

class TemplateDetailDataModel
{
    private const TABLE_NAME = 'template_detail';

    private const C_TEMPLATE_ID            = 'template_id';
    private const C_SEQ                    = 'seq';
    private const C_TYPE                   = 'type';
    private const C_AMOUNT                 = 'amount';
    private const C_ITEM                   = 'item';
    private const C_KIND_ELEMENT_ID        = 'kind_element_id';
    private const C_PURPOSE_ELEMENT_ID     = 'purpose_element_id';
    private const C_PLACE_ELEMENT_ID       = 'place_element_id';
    private const C_MOVE_BEFORE_PURPOSE_ID = 'move_before_purpose_id';
    private const C_MOVE_AFTER_PURPOSE_ID  = 'move_after_purpose_id';
    private const C_MOVE_BEFORE_PLACE_ID   = 'move_before_place_id';
    private const C_MOVE_AFTER_PLACE_ID    = 'move_after_place_id';

    public static function selectTemplateDetails(int $templateId): array
    {
        return DB::table(self::TABLE_NAME)
            ->where(self::C_TEMPLATE_ID, '=', $templateId)
            ->orderBy(self::C_SEQ)
            ->get()
            ->toArray();
    }

    public static function insertTemplateDetails(array $rows): void
    {
        DB::table(self::TABLE_NAME)->insert($rows);
    }

    public static function deleteTemplateDetails(int $templateId): void
    {
        DB::table(self::TABLE_NAME)
            ->where(self::C_TEMPLATE_ID, '=', $templateId)
            ->delete();
    }
}
