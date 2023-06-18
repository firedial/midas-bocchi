<?php

namespace App\Service;

use App\Models\Balance;
use App\Exceptions\InvalidParameterException;
use App\Models\Dao\BalanceDao;
use App\Models\Dao\Impl\BalanceDaoImpl;
use App\Models\Dao\MoveDao;
use App\Models\Dao\Impl\MoveDaoImpl;

/**
 * 給与操作のサービスクラス
 */
class SalaryService
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

    private $balanceDao;
    private $moveDao;

    public function __construct(BalanceDao $balanceDao = null, MoveDao $moveDao = null)
    {
        $this->balanceDao = $balanceDao ?: new BalanceDaoImpl();
        $this->moveDao = $moveDao ?: new MoveDaoImpl();
    }

    public function registerSalary(array $salary): Bool
    {
        \DB::beginTransaction();
        try {
            $baseSalary = [
                'amount' => (int)$salary['baseSalary'],
                'item' => '基本給',
                'kind_element_id' => self::SALARY_KIND_ELEMENT_ID,
                'purpose_element_id' => self::INCOME_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$salary['date']
            ];
            $this->balanceDao->insertBalance($baseSalary);

            $adjustmentSalary = [
                'amount' => (int)$salary['adjustmentSalary'],
                'item' => '職務調整給',
                'kind_element_id' => self::SALARY_KIND_ELEMENT_ID,
                'purpose_element_id' => self::INCOME_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$salary['date']
            ];
            $this->balanceDao->insertBalance($adjustmentSalary);

            $transportation = [
                'amount' => (int)$salary['transportation'],
                'item' => '非課税通勤費',
                'kind_element_id' => self::TRANSPORTATION_KIND_ELEMENT_ID,
                'purpose_element_id' => self::INCOME_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$salary['date']
            ];
            $this->balanceDao->insertBalance($transportation);

            $holdingIncentives = [
                'amount' => (int)$salary['holdingIncentives'],
                'item' => '持株奨励金',
                'kind_element_id' => self::HOLDING_KIND_ELEMENT_ID,
                'purpose_element_id' => self::INCOME_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$salary['date']
            ];
            $this->balanceDao->insertBalance($holdingIncentives);

            $transportationMove = [
                'amount' => (int)$salary['transportation'],
                'item' => '予算移動',
                'before_id' => self::INCOME_PURPOSE_ELEMENT_ID,
                'after_id' => self::TRANSPORTATION_PURPOSE_ELEMENT_ID,
                'date' => (string)$salary['date']
            ];
            $this->moveDao->insertMoveByArray('purpose', $transportationMove);

            $deductionValue =
                (int)$salary['healthInsurance'] +
                (int)$salary['welfarePension'] +
                (int)$salary['residentTax'] +
                (int)$salary['employmentInsurance'] +
                (int)$salary['incomeTax'] +
                (int)$salary['holding'];

            $deductionMove = [
                'amount' => $deductionValue,
                'item' => '予算移動',
                'before_id' => self::INCOME_PURPOSE_ELEMENT_ID,
                'after_id' => self::DEDUCTION_PURPOSE_ELEMENT_ID,
                'date' => (string)$salary['date']
            ];
            $this->moveDao->insertMoveByArray('purpose', $deductionMove);

            $healthInsurance = [
                'amount' => (-1) * (int)$salary['healthInsurance'],
                'item' => '健康保険料',
                'kind_element_id' => self::DEDUCTION_KIND_ELEMENT_ID,
                'purpose_element_id' => self::DEDUCTION_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$salary['date']
            ];
            $this->balanceDao->insertBalance($healthInsurance);

            $welfarePension = [
                'amount' => (-1) * (int)$salary['welfarePension'],
                'item' => '厚生年金保険',
                'kind_element_id' => self::DEDUCTION_KIND_ELEMENT_ID,
                'purpose_element_id' => self::DEDUCTION_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$salary['date']
            ];
            $this->balanceDao->insertBalance($welfarePension);

            $residentTax = [
                'amount' => (-1) * (int)$salary['residentTax'],
                'item' => '住民税',
                'kind_element_id' => self::DEDUCTION_KIND_ELEMENT_ID,
                'purpose_element_id' => self::DEDUCTION_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$salary['date']
            ];
            $this->balanceDao->insertBalance($residentTax);

            $employmentInsurance = [
                'amount' => (-1) * (int)$salary['employmentInsurance'],
                'item' => '雇用保険料',
                'kind_element_id' => self::DEDUCTION_KIND_ELEMENT_ID,
                'purpose_element_id' => self::DEDUCTION_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$salary['date']
            ];
            $this->balanceDao->insertBalance($employmentInsurance);

            $incomeTax = [
                'amount' => (-1) * (int)$salary['incomeTax'],
                'item' => '所得税',
                'kind_element_id' => self::DEDUCTION_KIND_ELEMENT_ID,
                'purpose_element_id' => self::DEDUCTION_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$salary['date']
            ];
            $this->balanceDao->insertBalance($incomeTax);

            $holding = [
                'amount' => (-1) * (int)$salary['holding'],
                'item' => '持株',
                'kind_element_id' => self::SHARE_HELD_KIND_ELEMENT_ID,
                'purpose_element_id' => self::DEDUCTION_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::SKY_PLACE_ELEMENT_ID,
                'date' => (string)$salary['date']
            ];
            $this->balanceDao->insertBalance($holding);

            $takeSalary =
                (int)$salary['baseSalary'] +
                (int)$salary['adjustmentSalary'] +
                (int)$salary['transportation'] +
                (int)$salary['holdingIncentives'] -
                $deductionValue;

            $mainMove = [
                'amount' => $takeSalary,
                'item' => '場所移動',
                'before_id' => self::SKY_PLACE_ELEMENT_ID,
                'after_id' => self::SALARY_PLACE_ELEMENT_ID,
                'date' => (string)$salary['date']
            ];
            $this->moveDao->insertMoveByArray('place', $mainMove);

            \DB::commit();
        } catch (Exception $e) {
            \DB::rollback();
        }

        return true;
    }

}
