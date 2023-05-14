<?php

namespace App\Models\Dao;

use Illuminate\Support\Facades\DB;

/**
 * 家計簿処理のレコードを取得するための Dao
 */
class BalanceDao
{

    public static function insertBalance(Array $balance)
    {
        return DB::table('m_balance')->insert($balance);
    }
}
