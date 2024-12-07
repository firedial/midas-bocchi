<?php

namespace App\Models\Dao\Impl;

use Illuminate\Support\Facades\DB;
use App\Models\Dao\SecretDao;

/**
 * 秘匿情報を登録するための Dao
 */
class SecretDaoImpl implements SecretDao
{
    const SECRET_ID = 1;

    public function updateSecret(array $value)
    {
        return DB::table('s_secret')->where('id', '=', self::SECRET_ID)->update($value);
    }

    public function selectSecret()
    {
        return DB::table('s_secret')->where('id', '=', self::SECRET_ID)->select()->get()->toArray();
    }
}
