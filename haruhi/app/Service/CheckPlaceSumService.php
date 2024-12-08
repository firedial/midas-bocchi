<?php

namespace App\Service;

use App\Models\Dao\CheckPlaceSumDao;
use App\Models\Dao\Impl\CheckPlaceSumDaoImpl;

/**
 * 金額確認のサービスクラス
 */
class CheckPlaceSumService
{
    private $checkPlaceSumDao;

    public function __construct(CheckPlaceDao $checkPlaceSumDao = null)
    {
        $this->checkPlaceSumDao = $checkPlaceSumDao ?: new CheckPlaceSumDaoImpl();
    }

    public function registerCheckPlaceSum(array $data): bool
    {
        \DB::beginTransaction();
        try {
            $value = [
                'sum' => $data['sum'],
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
