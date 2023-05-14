<?php

namespace App\Service;

use App\Models\Balance;
use App\Util\Date;
use App\Exceptions\InvalidParameterException;
use App\Models\Dao\MoveDao;
use App\Models\Dao\BalanceDao;

/**
 * 賞与操作のサービスクラス
 */
class BonusService
{
    const SALARY_KIND_ELEMENT_ID = 14;
    const TRANSPORTATION_KIND_ELEMENT_ID = 17;
    const HOLDING_KIND_ELEMENT_ID = 18;
    const DEDUCTION_KIND_ELEMENT_ID = 16;
    const SHARE_HELD_KIND_ELEMENT_ID = 19;

    const INCOME_PURPOSE_ELEMENT_ID = 3;
    const TRANSPORTATION_PURPOSE_ELEMENT_ID = 4;
    const DEDUCTION_PURPOSE_ELEMENT_ID = 14;

    const SKY_PLACE_ELEMENT_ID = 4;
    const SALARY_PLACE_ELEMENT_ID = 8;

    public static function registerBonus(array $bonus): Bool
    {
        \DB::beginTransaction();
        try {
            $bonusValue = [
                'amount' => (int)$bonus['bonus'],
                'item' => 'ボーナス',
                'kind_element_id' => self::SALARY_KIND_ELEMENT_ID,
                'purpose_element_id' => self::INCOME_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$bonus['date']
            ];
            BalanceDao::insertBalance($bonusValue);

            $deductionValue =
                (int)$bonus['healthInsurance'] +
                (int)$bonus['welfarePension'] +
                (int)$bonus['employmentInsurance'] +
                (int)$bonus['incomeTax'];

            $deductionMove = [
                'amount' => $deductionValue,
                'item' => '予算移動',
                'before_id' => self::INCOME_PURPOSE_ELEMENT_ID,
                'after_id' => self::DEDUCTION_PURPOSE_ELEMENT_ID,
                'date' => (string)$bonus['date']
            ];
            MoveDao::insertMoveByArray('purpose', $deductionMove);

            $healthInsurance = [
                'amount' => (-1) * (int)$bonus['healthInsurance'],
                'item' => '健康保険料',
                'kind_element_id' => self::DEDUCTION_KIND_ELEMENT_ID,
                'purpose_element_id' => self::DEDUCTION_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$bonus['date']
            ];
            BalanceDao::insertBalance($healthInsurance);

            $welfarePension = [
                'amount' => (-1) * (int)$bonus['welfarePension'],
                'item' => '厚生年金保険',
                'kind_element_id' => self::DEDUCTION_KIND_ELEMENT_ID,
                'purpose_element_id' => self::DEDUCTION_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$bonus['date']
            ];
            BalanceDao::insertBalance($welfarePension);

            $employmentInsurance = [
                'amount' => (-1) * (int)$bonus['employmentInsurance'],
                'item' => '雇用保険料',
                'kind_element_id' => self::DEDUCTION_KIND_ELEMENT_ID,
                'purpose_element_id' => self::DEDUCTION_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$bonus['date']
            ];
            BalanceDao::insertBalance($employmentInsurance);

            $incomeTax = [
                'amount' => (-1) * (int)$bonus['incomeTax'],
                'item' => '所得税',
                'kind_element_id' => self::DEDUCTION_KIND_ELEMENT_ID,
                'purpose_element_id' => self::DEDUCTION_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$bonus['date']
            ];
            BalanceDao::insertBalance($incomeTax);

            $takeBonus = (int)$bonus['bonus'] - $deductionValue;

            $mainMove = [
                'amount' => $takeBonus,
                'item' => '場所移動',
                'before_id' => self::SKY_PLACE_ELEMENT_ID,
                'after_id' => self::SALARY_PLACE_ELEMENT_ID,
                'date' => (string)$bonus['date']
            ];
            MoveDao::insertMoveByArray('place', $mainMove);

            \DB::commit();
        } catch (Exception $e) {
            \DB::rollback();
        }

        return true;
    }

}
