<?php

namespace App\Service;

use App\Models\Balance;
use App\Exceptions\InvalidParameterException;
use App\Models\Dao\BalanceDao;
use App\Models\Dao\Impl\BalanceDaoImpl;

/**
 * 賞与操作のサービスクラス
 */
class MonthlyService
{
    const HOUSE_RENT_KIND_ELEMENT_ID = 8;
    const GAS_KIND_ELEMENT_ID = 9;
    const WATER_KIND_ELEMENT_ID = 10;
    const ELECT_KIND_ELEMENT_ID = 11;
    const NET_KIND_ELEMENT_ID = 12;

    const UTIL_COST_PURPOSE_ELEMENT_ID = 10;

    const WITHDRAWAL_PLACE_ELEMENT_ID = 9;

    private $balanceDao;

    public function __construct(BalanceDao $balanceDao = null)
    {
        $this->balanceDao = $balanceDao ?: new BalanceDaoImpl();
    }

    public function registerMonthly(array $data): Bool
    {
        \DB::beginTransaction();
        try {
            if ((int)$data['houseRent']['amount'] !== 0) {
                $value = [
                    'amount' => (-1) * (int)$data['houseRent']['amount'],
                    'item' => '家賃',
                    'kind_element_id' => self::HOUSE_RENT_KIND_ELEMENT_ID,
                    'purpose_element_id' => self::UTIL_COST_PURPOSE_ELEMENT_ID,
                    'place_element_id' => self::WITHDRAWAL_PLACE_ELEMENT_ID,
                    'date' => (string)$data['houseRent']['date']
                ];
                $this->balanceDao->insertBalance($value);
            }

            if ((int)$data['gas']['amount'] !== 0) {
                $value = [
                    'amount' => (-1) * (int)$data['gas']['amount'],
                    'item' => 'ガス代',
                    'kind_element_id' => self::GAS_KIND_ELEMENT_ID,
                    'purpose_element_id' => self::UTIL_COST_PURPOSE_ELEMENT_ID,
                    'place_element_id' => self::WITHDRAWAL_PLACE_ELEMENT_ID,
                    'date' => (string)$data['gas']['date']
                ];
                $this->balanceDao->insertBalance($value);
            }

            if ((int)$data['water']['amount'] !== 0) {
                $value = [
                    'amount' => (-1) * (int)$data['water']['amount'],
                    'item' => '水道代',
                    'kind_element_id' => self::WATER_KIND_ELEMENT_ID,
                    'purpose_element_id' => self::UTIL_COST_PURPOSE_ELEMENT_ID,
                    'place_element_id' => self::WITHDRAWAL_PLACE_ELEMENT_ID,
                    'date' => (string)$data['water']['date']
                ];
                $this->balanceDao->insertBalance($value);
            }

            if ((int)$data['elect']['amount'] !== 0) {
                $value = [
                    'amount' => (-1) * (int)$data['elect']['amount'],
                    'item' => '電気代',
                    'kind_element_id' => self::ELECT_KIND_ELEMENT_ID,
                    'purpose_element_id' => self::UTIL_COST_PURPOSE_ELEMENT_ID,
                    'place_element_id' => self::WITHDRAWAL_PLACE_ELEMENT_ID,
                    'date' => (string)$data['elect']['date']
                ];
                $this->balanceDao->insertBalance($value);
            }

            if ((int)$data['net']['amount'] !== 0) {
                $value = [
                    'amount' => (-1) * (int)$data['net']['amount'],
                    'item' => 'ネット代',
                    'kind_element_id' => self::NET_KIND_ELEMENT_ID,
                    'purpose_element_id' => self::UTIL_COST_PURPOSE_ELEMENT_ID,
                    'place_element_id' => self::WITHDRAWAL_PLACE_ELEMENT_ID,
                    'date' => (string)$data['net']['date']
                ];
                $this->balanceDao->insertBalance($value);
            }

            \DB::commit();
        } catch (Exception $e) {
            \DB::rollback();
        }

        return true;
    }

}
