<?php

namespace Mock;
use App\Models\Dao\BalanceDao;

class BalanceDaoImpl implements BalanceDao
{
    public function selectBalance(array $params)
    {
        return [
            [
                'id' => 1,
                'amount' => 21,
                'item' => "item_name",
                'kind_element_id' => "kind",
                'purpose_element_id' => "purpose",
                'place_element_id' => "place",
                'date' => "2024/10/10",
                'kind_element_description' => "kind_desc",
                'purpose_element_description' => "purpose_desc",
                'place_element_description' => "place_desc",
            ]
        ];
    }

    public function insertBalance(array $balance)
    {
        return 1;
    }

    public function updateBalance(array $balance)
    {
        return 1;
    }

    public function deleteBalance(int $id)
    {
        return 1;
    }

}
