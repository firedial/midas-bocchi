<?php

namespace App\Models\DataModels;

use Illuminate\Support\Facades\DB;
use stdClass;

class ScenarioDataModel
{
    private const TABLE_NAME = 'scenario';

    private const C_ID   = 'id';
    private const C_NAME = 'name';

    public static function selectScenario(?int $id = null): array
    {
        $query = DB::table(self::TABLE_NAME)
            ->select(self::C_ID, self::C_NAME)
            ->orderBy(self::C_ID);

        if (!is_null($id)) {
            $query->where(self::C_ID, '=', $id);
        }

        return $query->get()->toArray();
    }

    public static function insertScenario(string $name): stdClass
    {
        return DB::selectOne(
            'INSERT INTO ' . self::TABLE_NAME . ' (' . self::C_NAME . ') VALUES (?) RETURNING *',
            [$name]
        );
    }

    public static function updateScenario(int $id, string $name): stdClass
    {
        DB::table(self::TABLE_NAME)
            ->where(self::C_ID, '=', $id)
            ->update([self::C_NAME => $name]);

        return DB::selectOne(
            'SELECT * FROM ' . self::TABLE_NAME . ' WHERE ' . self::C_ID . ' = ?',
            [$id]
        );
    }

    public static function deleteScenario(int $id): stdClass
    {
        return DB::selectOne(
            'DELETE FROM ' . self::TABLE_NAME . ' WHERE ' . self::C_ID . ' = ? RETURNING *',
            [$id]
        );
    }
}
