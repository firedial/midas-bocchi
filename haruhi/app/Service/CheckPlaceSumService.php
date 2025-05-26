<?php

namespace App\Service;

use App\Models\Dao\CheckPlaceSumDao;
use App\Models\Dao\Impl\CheckPlaceSumDaoImpl;
use App\Models\Dao\BalanceDao;
use App\Models\Dao\Impl\BalanceDaoImpl;

/**
 * 金額確認のサービスクラス
 */
class CheckPlaceSumService
{
    private $checkPlaceSumDao;

    public function __construct(CheckPlaceDao $checkPlaceSumDao = null, BalanceDao $balanceDao = null)
    {
        $this->checkPlaceSumDao = $checkPlaceSumDao ?: new CheckPlaceSumDaoImpl();
        $this->balanceDao = $balanceDao ?: new BalanceDaoImpl();
    }

    public function registerCheckPlaceSum(array $data): bool
    {
        \DB::beginTransaction();
        try {
            $sum = $this->balanceDao->getSum($data['placeElementId']);

            $value = [
                'sum' => $sum,
                'place_element_id' => $data['placeElementId'],
                'date' => $data['date'],
            ];
            $this->checkPlaceSumDao->insertCheckPlaceSum($value);

            \DB::commit();
        } catch (Exception $e) {
            \DB::rollback();
        }

        return true;
    }

}
