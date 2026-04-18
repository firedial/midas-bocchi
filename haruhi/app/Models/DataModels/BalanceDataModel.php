<?php

namespace App\Models\DataModels;

use Illuminate\Support\Facades\DB;
use stdClass;

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
    private const C_GROUP_ID = "group_id";

    public static function selectBalance(
        ?int $notKindElementId = null,
        ?int $purposeElementId = null,
        ?int $placeElementId = null,
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
                self::TABLE_NAME . '.' . self::C_GROUP_ID,
                KindElementDataModel::TABLE_NAME . '.' . KindElementDataModel::C_DESCRIPTION . ' AS kind_element_description',
                PurposeElementDataModel::TABLE_NAME . '.' . PurposeElementDataModel::C_DESCRIPTION . ' AS purpose_element_description',
                PlaceElementDataModel::TABLE_NAME . '.' . PlaceElementDataModel::C_DESCRIPTION . ' AS place_element_description',
            )
            ->join(KindElementDataModel::TABLE_NAME, KindElementDataModel::TABLE_NAME . '.' . KindElementDataModel::C_ID, '=', self::TABLE_NAME . '.' . self::C_KIND_ELEMENT_ID)
            ->join(PurposeElementDataModel::TABLE_NAME, PurposeElementDataModel::TABLE_NAME . '.' . PurposeElementDataModel::C_ID, '=', self::TABLE_NAME . '.' . self::C_PURPOSE_ELEMENT_ID)
            ->join(PlaceElementDataModel::TABLE_NAME, PlaceElementDataModel::TABLE_NAME . '.' . PlaceElementDataModel::C_ID, '=', self::TABLE_NAME . '.' . self::C_PLACE_ELEMENT_ID);
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
        if ($purposeElementId) {
            $query->where(self::TABLE_NAME . '.' . self::C_PURPOSE_ELEMENT_ID, '=', $purposeElementId);
        }
        if ($placeElementId) {
            $query->where(self::TABLE_NAME . '.' . self::C_PLACE_ELEMENT_ID, '=', $placeElementId);
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
        ?int $groupId = null,
    ): stdClass {
        $result = DB::selectOne(
            'INSERT INTO ' . self::TABLE_NAME . ' (' .
                self::C_AMOUNT . ', ' .
                self::C_ITEM . ', ' .
                self::C_KIND_ELEMENT_ID . ', ' .
                self::C_PURPOSE_ELEMENT_ID . ', ' .
                self::C_PLACE_ELEMENT_ID . ', ' .
                self::C_DATE . ', ' .
                self::C_GROUP_ID .
                ') VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING *',
            [$amount, $item, $kindElementId, $purposeElementId, $placeElementId, $date, $groupId]
        );

        if (is_null($groupId)) {
            DB::table(self::TABLE_NAME)
                ->where(self::C_ID, '=', $result->id)
                ->update([self::C_GROUP_ID => $result->id]);
            $result->group_id = $result->id;
        }

        return $result;
    }

    public static function updateBalance(
        int $id,
        int $amount,
        string $item,
        int $kindElementId,
        int $purposeElementId,
        int $placeElementId,
        string $date,
        int $groupId,
    ): stdClass {
        DB::table(self::TABLE_NAME)
            ->where(self::C_ID, '=', $id)
            ->update([
                self::C_AMOUNT => $amount,
                self::C_ITEM => $item,
                self::C_KIND_ELEMENT_ID => $kindElementId,
                self::C_PURPOSE_ELEMENT_ID => $purposeElementId,
                self::C_PLACE_ELEMENT_ID => $placeElementId,
                self::C_DATE => $date,
                self::C_GROUP_ID => $groupId,
            ]);

        return DB::selectOne(
            'SELECT * FROM ' . self::TABLE_NAME . ' WHERE ' . self::C_ID . ' = ?',
            [$id]
        );
    }

    public static function deleteBalance(
        int $id,
    ): stdClass {
        return DB::selectOne(
            'DELETE FROM ' . self::TABLE_NAME . ' WHERE ' . self::C_ID . ' = ? RETURNING *',
            [$id]
        );
    }

    public static function sum(
        int $placeElementId,
    ): int {
        return DB::table(self::TABLE_NAME)
            ->where(self::C_PLACE_ELEMENT_ID, '=', $placeElementId)
            ->sum(self::C_AMOUNT);
    }
}
