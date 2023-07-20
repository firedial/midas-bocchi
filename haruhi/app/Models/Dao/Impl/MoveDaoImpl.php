<?php

namespace App\Models\Dao\Impl;

use Illuminate\Support\Facades\DB;
use Illuminate\Http\Request;
use App\Models\KindElement;
use App\Models\PurposeElement;
use App\Models\PlaceElement;
use App\Models\Dao\MoveDao;

/**
 * 移動処理のレコードを取得するための Dao
 */
class MoveDaoImpl implements MoveDao
{
    /**
     * 移動処理のレコードを取得する
     *
     * 降順で20件取得する
     *
     * @param String $attributeName 移動処理を取得したい属性名
     * @return array 移動処理のレコード
     */

    public function getMoves(String $attributeName)
    {
        $moveAttributeConditionColumn = '';
        $moveAttributeConditionId = '';
        if ($attributeName === 'purpose') {
            $moveAttributeConditionColumn = 'before.place_element_id';
            $moveAttributeConditionId = PlaceElement::MOVE_ID;
        } else if ($attributeName === 'place') {
            $moveAttributeConditionColumn = 'before.purpose_element_id';
            $moveAttributeConditionId = PurposeElement::MOVE_ID;
        } else {
            // ここにはこない想定
            // exception 吐いたほうがいい
            // もっと上位で処理しても良さそう
        }

        return DB::table('m_balance as before')
            ->select(
                'before.id AS id',
                'after.amount AS amount',
                'before.item AS item',
                'before.' . $attributeName . '_element_id AS before_id',
                'after.' . $attributeName . '_element_id AS after_id',
                'before.date AS date',
                'before_attribute_element.description AS before_description',
                'after_attribute_element.description AS after_description'
            )
            ->join('m_balance as after', 'after.id', '=', DB::raw('before.id + 1'))
            ->join('m_' . $attributeName . '_element as before_attribute_element', 'before_attribute_element.id', '=', 'before.' . $attributeName . '_element_id')
            ->join('m_' . $attributeName . '_element as after_attribute_element', 'after_attribute_element.id', '=', 'after.' . $attributeName . '_element_id')
            ->where('before.kind_element_id', KindElement::MOVE_ID)
            ->where('before.amount', '<', 0)
            ->where($moveAttributeConditionColumn, $moveAttributeConditionId)
            ->limit(20)
            ->orderby('id', 'desc')
            ->get()
            ->toArray();
    }

    /**
     * ID で指定された移動処理を取得する
     *
     * @param String $attributeName 移動処理を取得したい属性名
     * @param Int 取得する主キー
     * @return array 指定された移動処理のレコード
     */
    public function getMoveById(String $attributeName, Int $id)
    {
        $list = DB::table('m_balance as before')
            ->select(
                'before.id AS id',
                'after.amount AS amount',
                'before.item AS item',
                'before.' . $attributeName . '_element_id AS before_id',
                'after.' . $attributeName . '_element_id AS after_id',
                'before.date AS date',
                'before_attribute_element.description AS before_description',
                'after_attribute_element.description AS after_description'
            )
            ->join('m_balance as after', 'after.id', '=', DB::raw('before.id + 1'))
            ->join('m_' . $attributeName . '_element as before_attribute_element', 'before_attribute_element.id', '=', 'before.' . $attributeName . '_element_id')
            ->join('m_' . $attributeName . '_element as after_attribute_element', 'after_attribute_element.id', '=', 'after.' . $attributeName . '_element_id')
            ->where('before.kind_element_id', KindElement::MOVE_ID)
            ->where('before.amount', '<', 0)
            ->where('before.id', $id)
            ->get()
            ->toArray();

        return (array)$list[0];
    }
}
