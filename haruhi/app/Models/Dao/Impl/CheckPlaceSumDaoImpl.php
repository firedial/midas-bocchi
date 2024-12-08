<?php

namespace App\Models\Dao\Impl;

use Illuminate\Support\Facades\DB;
use App\Models\Dao\CheckPlaceSumDao;

/**
 * 金額確認を登録するための Dao
 */
class CheckPlaceSumDaoImpl implements CheckPlaceSumDao
{
    public function insertCheckPlaceSum(array $value)
    {
        return DB::table('r_check_place_sum')->insert($value);
    }
}
