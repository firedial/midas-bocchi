<?php

namespace App\Service;

use App\Models\KindElement;
use App\Models\PurposeElement;
use App\Models\PlaceElement;
use App\Models\Balance;
use App\Util\Date;
use App\Exceptions\InvalidParameterException;

/**
 * balance テーブルの操作のサービスクラス
 */
class BalanceService
{

    public static function createBalance(Balance $balance): Bool
    {
        // 登録処理なので id は空である
        if (!is_null($balance->id)) {
            throw new InvalidParameterException('Id is not null.');
        }

        self::validation($balance);


        return $balance->save();
    }

    public static function deleteBalance(Balance $balance): Bool
    {
        return $balance->delete();
    }

    private static function validation(Balance $balance): void
    {
        // 移動処理を表す id が入っていた場合は不正
        if ($balance->kind_element_id === KindElement::MOVE_ID) {
            throw new InvalidParameterException('Kind element id is move id.');
        }
        if ($balance->purpose_element_id === PurposeElement::MOVE_ID) {
            throw new InvalidParameterException('Purpose element id is move id.');
        }
        if ($balance->place_element_id === PlaceElement::MOVE_ID) {
            throw new InvalidParameterException('Place element id is move id.');
        }

        // 金額は 0 でない値じゃないとダメ
        if ($balance->amount === 0) {
            throw new InvalidParameterException('Amount is zero.');
        }

        // 項目は空文字ではないとダメ
        if ($balance->item === '') {
            throw new InvalidParameterException('Item is empty.');
        }

        // 日付が正しい形式か
        if (!Date::isValidDateString($balance->date)) {
            // @todo 現状スラッシュ区切りで渡されているので、フロント側を修正する必要あり
            // throw new InvalidParameterException('Date is invalid.');
        }
    }
}
