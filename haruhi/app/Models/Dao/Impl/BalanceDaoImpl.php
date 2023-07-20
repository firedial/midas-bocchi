<?php

namespace App\Models\Dao\Impl;

use Illuminate\Support\Facades\DB;
use App\Models\Dao\BalanceDao;
use App\Models\KindElement;

/**
 * 家計簿処理のレコードを取得するための Dao
 */
class BalanceDaoImpl implements BalanceDao
{
    public function selectBalance(array $params)
    {
        $query = DB::table('m_balance')
            ->select(
                'm_balance.id AS id',
                'm_balance.amount AS amount',
                'm_balance.item AS item',
                'm_balance.kind_element_id AS kind_element_id',
                'm_balance.purpose_element_id AS purpose_element_id',
                'm_balance.place_element_id AS place_element_id',
                'm_balance.date AS date',
                'm_kind_element.description AS kind_element_description',
                'm_purpose_element.description AS purpose_element_description',
                'm_place_element.description AS place_element_description'
            )
            ->join('m_kind_element', 'm_kind_element.id', '=', 'm_balance.kind_element_id')
            ->join('m_purpose_element', 'm_purpose_element.id', '=', 'm_balance.purpose_element_id')
            ->join('m_place_element', 'm_place_element.id', '=', 'm_balance.place_element_id')
            ->where('m_balance.kind_element_id', '<>', KindElement::MOVE_ID);
        if (is_numeric($params['limit'])) {
            $query->limit($params['limit']);
        }
        // @todo クエリそのまま入れているので後で修正する
        if (!is_null($params['orderby'])) {
            $query->orderby('m_balance.id', $params['orderby']);
        }
        if (!is_null($params['id'])) {
            $query->where('m_balance.id', '=', $params['id']);
        }

        return $query->get()->toArray();

    }

    public function insertBalance(array $balance)
    {
        return DB::table('m_balance')->insert($balance);
    }

    public function updateBalance(array $balance)
    {
        // @todo 移動処理じゃないことを確認したほうがいい(移動処理でもこの関数使うのでもっと上位で)
        return DB::table('m_balance')->where('id', '=', $balance['id'])->update($balance);
    }

    public function deleteBalance(int $id)
    {
        // @todo 移動処理じゃないことを確認したほうがいい(移動処理でもこの関数使うのでもっと上位で)
        return DB::table('m_balance')->where('id', '=', $id)->delete();
    }

}
