<?php

namespace App\Util;

class Date {
    
    /**
     * 日付の形式が正しいかをチェックする
     * 
     * ハイフン区切りでありえない日付でないかをみる
     * 正しい例: 2021-2-5, 2020-2-28, 2021-02-3
     * 不正: 2021/2/5, 2021-2-29, 2021-2-6-3, 2021-1
     */
    public static function isValidDateString(string $date): bool
    {
        $d = explode('-', $date);
        if (count($d) !== 3) {
            return false;
        }
        
        $day = (int)$d[0];
        $month = (int)$d[1];
        $year = (int)$d[2];

        return checkdate($month, $day, $year);
    }
}
