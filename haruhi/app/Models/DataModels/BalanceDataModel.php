<?php

namespace App\Models\DataModels;

use Illuminate\Support\Facades\DB;

class BalanceDataModel
{
    private const TABLE_NAME = "m_balance";

    private const C_ID = "id";
    private const C_AMOUNT = "amount";
    private const C_ITEM = "item";
    private const C_KIND_ELEMENT_ID = "kind_element_id";
    private const C_PURPOSE_ELEMENT_ID = "purpose_element_id";
    private const C_PLACE_ELEMENT_ID = "place_element_id";
    private const C_DATE = "date";

    private const KIND_ELEMENT_MOVE_ID = 1;

    public static function selectBalance(
        bool $isNotMoveOnly = false,
        bool $isMoveOnly = false,
        ?int $id = null,
        ?int $limit = null,
        ?string $orderBy = null,
    ): array {
        $query = DB::table('m_balance')
            ->select(
                'm_balance.id AS id',
                'm_balance.amount AS amount',
                'm_balance.item AS item',
                'm_balance.kind_element_id AS kind_element_id',
                'm_balance.purpose_element_id AS purpose_element_id',
                'm_balance.place_element_id AS place_element_id',
                'm_balance.date AS date',
                'm_kind_element.description AS kind_element_description',
                'm_purpose_element.description AS purpose_element_description',
                'm_place_element.description AS place_element_description'
            )
            ->join('m_kind_element', 'm_kind_element.id', '=', 'm_balance.kind_element_id')
            ->join('m_purpose_element', 'm_purpose_element.id', '=', 'm_balance.purpose_element_id')
            ->join('m_place_element', 'm_place_element.id', '=', 'm_balance.place_element_id');
        if (!is_null($limit)) {
            $query->limit($limit);
        }
        if (!is_null($orderBy)) {
            $query->orderby('m_balance.id', $orderBy);
        }
        if (!is_null($id)) {
            $query->where('m_balance.id', '=', $id);
        }
        if ($isNotMoveOnly) {
            $query->where('m_balance.kind_element_id', '<>', self::KIND_ELEMENT_MOVE_ID);
        }
        if ($isMoveOnly) {
            $query->where('m_balance.kind_element_id', '=', self::KIND_ELEMENT_MOVE_ID);
        }

        return $query->get()->toArray();
    }

    public static function insertBalance(
        int $amount,
        string $item,
        int $kindElementId,
        int $purposeElementId,
        int $placeElementId,
        string $date,
    ): int {
        return DB::table(self::TABLE_NAME)
            ->insertGetId([
                self::C_AMOUNT => $amount,
                self::C_ITEM => $item,
                self::C_KIND_ELEMENT_ID => $kindElementId,
                self::C_PURPOSE_ELEMENT_ID => $purposeElementId,
                self::C_PLACE_ELEMENT_ID => $placeElementId,
                self::C_DATE => $date,
            ]);
    }

    public static function updateBalance(
        int $id,
        int $amount,
        string $item,
        int $kindElementId,
        int $purposeElementId,
        int $placeElementId,
        string $date,
    ): void {
        DB::table(self::TABLE_NAME)
            ->where(self::C_ID, '=', $id)
            ->update([
                self::C_AMOUNT => $amount,
                self::C_ITEM => $item,
                self::C_KIND_ELEMENT_ID => $kindElementId,
                self::C_PURPOSE_ELEMENT_ID => $purposeElementId,
                self::C_PLACE_ELEMENT_ID => $placeElementId,
                self::C_DATE => $date,
            ]);
    }

    public static function deleteBalance(
        int $id,
    ): void {
        DB::table(self::TABLE_NAME)
            ->where(self::C_ID, '=', $id)
            ->delete();
    }
}
