<?php

namespace App\Models\DataModels;

use Illuminate\Support\Facades\DB;

class TemplateDetailDataModel
{
    private const TABLE_NAME = 'template_detail';

    private const C_TEMPLATE_ID = 'template_id';
    private const C_SEQ = 'seq';

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
