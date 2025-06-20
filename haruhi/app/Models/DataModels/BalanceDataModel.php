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

    public static function selectBalance(
        ?int $notKindElementId = null,
        ?int $kindElementId = null,
        ?int $id = null,
        ?int $limit = null,
        ?bool $orderByDesc = null,
    ): array {
        $query = DB::table(self::TABLE_NAME)
            ->select(
                self::TABLE_NAME . '.' . self::C_ID,
                self::TABLE_NAME . '.' . self::C_AMOUNT,
                self::TABLE_NAME . '.' . self::C_ITEM,
                self::TABLE_NAME . '.' . self::C_KIND_ELEMENT_ID,
                self::TABLE_NAME . '.' . self::C_PURPOSE_ELEMENT_ID,
                self::TABLE_NAME . '.' . self::C_PLACE_ELEMENT_ID,
                self::TABLE_NAME . '.' . self::C_DATE,
                'm_kind_element.description AS kind_element_description',
                'm_purpose_element.description AS purpose_element_description',
                'm_place_element.description AS place_element_description'
            )
            ->join('m_kind_element', 'm_kind_element.id', '=', self::TABLE_NAME . '.' . self::C_KIND_ELEMENT_ID)
            ->join('m_purpose_element', 'm_purpose_element.id', '=', self::TABLE_NAME . '.' . self::C_PURPOSE_ELEMENT_ID)
            ->join('m_place_element', 'm_place_element.id', '=', self::TABLE_NAME . '.' . self::C_PLACE_ELEMENT_ID);
        if (!is_null($limit)) {
            $query->limit($limit);
        }
        if (!is_null($orderByDesc)) {
            $query->orderby(self::TABLE_NAME . '.' . self::C_ID, 'desc');
        }
        if (!is_null($id)) {
            $query->where(self::TABLE_NAME . '.' . self::C_ID, '=', $id);
        }
        if ($notKindElementId) {
            $query->where(self::TABLE_NAME . '.' . self::C_KIND_ELEMENT_ID, '<>', $notKindElementId);
        }
        if ($kindElementId) {
            $query->where(self::TABLE_NAME . '.' . self::C_KIND_ELEMENT_ID, '=', $kindElementId);
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
