<?php

namespace App\Service;

use App\Models\Balance;
use App\Models\Dao\BalanceDao;
use App\Models\Dao\Impl\BalanceDaoImpl;
use App\Models\Dao\SecretDao;
use App\Models\Dao\Impl\SecretDaoImpl;

/**
 * 会社交通費のサービスクラス
 */
class TransportationService
{
    const TRANSPORTATION = 'officeTransportation';
    const TRANSPORTATION_KIND_ELEMENT_ID = 20;
    const TRANSPORTATION_PURPOSE_ELEMENT_ID = 4;
    const TRANSPORTATION_PLACE_ELEMENT_ID = 44;

    private $balanceDao;

    public function __construct(BalanceDao $balanceDao = null, SecretDao $secretDao = null)
    {
        $this->balanceDao = $balanceDao ?: new BalanceDaoImpl();
        $this->secretDao = $secretDao ?: new SecretDaoImpl();
    }

    public function registerTransportation(string $date): bool
    {
        \DB::beginTransaction();
        try {
            $transportation = (int)json_decode($this->secretDao->selectSecret()[0]->value, true)[self::TRANSPORTATION];

            $value = [
                'amount' => (-1) * $transportation,
                'item' => '交通費',
                'kind_element_id' => self::TRANSPORTATION_KIND_ELEMENT_ID,
                'purpose_element_id' => self::TRANSPORTATION_PURPOSE_ELEMENT_ID,
                'place_element_id' => self::TRANSPORTATION_PLACE_ELEMENT_ID,
                'date' => $date,
            ];
            $this->balanceDao->insertBalance($value);

            \DB::commit();
        } catch (Exception $e) {
            \DB::rollback();
        }

        return true;
    }

}
