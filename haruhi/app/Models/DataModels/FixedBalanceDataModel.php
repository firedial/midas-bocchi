<?php

namespace App\Models\DataModels;

use Illuminate\Support\Facades\DB;

class FixedBalanceDataModel
{
    private const TABLE_NAME = "m_fixed_balance";

    private const C_ID = "id";
    private const C_AMOUNT = "amount";
    private const C_ITEM = "item";
    private const C_KIND_ELEMENT_ID = "kind_element_id";
    private const C_PURPOSE_ELEMENT_ID = "purpose_element_id";
    private const C_PLACE_ELEMENT_ID = "place_element_id";

    public static function selectFixedBalance(
        ?int $id = null,
    ): array {
        $query = DB::table(self::TABLE_NAME)
            ->select(
                self::TABLE_NAME . '.' . self::C_ID,
                self::TABLE_NAME . '.' . self::C_AMOUNT,
                self::TABLE_NAME . '.' . self::C_ITEM,
                self::TABLE_NAME . '.' . self::C_KIND_ELEMENT_ID,
                self::TABLE_NAME . '.' . self::C_PURPOSE_ELEMENT_ID,
                self::TABLE_NAME . '.' . self::C_PLACE_ELEMENT_ID,
                KindElementDataModel::TABLE_NAME . '.' . KindElementDataModel::C_DESCRIPTION . ' AS kind_element_description',
                PurposeElementDataModel::TABLE_NAME . '.' . PurposeElementDataModel::C_DESCRIPTION . ' AS purpose_element_description',
                PlaceElementDataModel::TABLE_NAME . '.' . PlaceElementDataModel::C_DESCRIPTION . ' AS place_element_description',
            )
            ->join(KindElementDataModel::TABLE_NAME, KindElementDataModel::TABLE_NAME . '.' . KindElementDataModel::C_ID, '=', self::TABLE_NAME . '.' . self::C_KIND_ELEMENT_ID)
            ->join(PurposeElementDataModel::TABLE_NAME, PurposeElementDataModel::TABLE_NAME . '.' . PurposeElementDataModel::C_ID, '=', self::TABLE_NAME . '.' . self::C_PURPOSE_ELEMENT_ID)
            ->join(PlaceElementDataModel::TABLE_NAME, PlaceElementDataModel::TABLE_NAME . '.' . PlaceElementDataModel::C_ID, '=', self::TABLE_NAME . '.' . self::C_PLACE_ELEMENT_ID);
        $query->orderby(self::TABLE_NAME . '.' . self::C_ID, 'asc');
        if (!is_null($id)) {
            $query->where(self::TABLE_NAME . '.' . self::C_ID, '=', $id);
        }
        return $query->get()->toArray();
    }

    public static function insertFixedBalance(
        int $amount,
        string $item,
        int $kindElementId,
        int $purposeElementId,
        int $placeElementId,
    ): int {
        return DB::table(self::TABLE_NAME)
            ->insertGetId([
                self::C_AMOUNT => $amount,
                self::C_ITEM => $item,
                self::C_KIND_ELEMENT_ID => $kindElementId,
                self::C_PURPOSE_ELEMENT_ID => $purposeElementId,
                self::C_PLACE_ELEMENT_ID => $placeElementId,
            ]);
    }

    public static function updateFixedBalance(
        int $id,
        int $amount,
        string $item,
        int $kindElementId,
        int $purposeElementId,
        int $placeElementId,
    ): void {
        DB::table(self::TABLE_NAME)
            ->where(self::C_ID, '=', $id)
            ->update([
                self::C_AMOUNT => $amount,
                self::C_ITEM => $item,
                self::C_KIND_ELEMENT_ID => $kindElementId,
                self::C_PURPOSE_ELEMENT_ID => $purposeElementId,
                self::C_PLACE_ELEMENT_ID => $placeElementId,
            ]);
    }

    public static function deleteFixedBalance(
        int $id,
    ): void {
        DB::table(self::TABLE_NAME)
            ->where(self::C_ID, '=', $id)
            ->delete();
    }
}
