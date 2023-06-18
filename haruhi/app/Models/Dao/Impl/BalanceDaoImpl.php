<?php

namespace App\Models\Dao\Impl;

use Illuminate\Support\Facades\DB;
use App\Models\Dao\BalanceDao;

/**
 * 家計簿処理のレコードを取得するための Dao
 */
class BalanceDaoImpl implements BalanceDao
{

    public static function insertBalance(Array $balance)
    {
        return DB::table('m_balance')->insert($balance);
    }
}
