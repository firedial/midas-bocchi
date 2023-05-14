<?php

namespace App\Models\Dao;

use Illuminate\Support\Facades\DB;
use Illuminate\Http\Request;
use App\Models\KindElement;
use App\Models\PurposeElement;
use App\Models\PlaceElement;

/**
 * 移動処理のレコードを取得するための Dao
 */
class MoveDao
{
    /**
     * 移動処理のレコードを取得する
     * 
     * 降順で20件取得する
     * 
     * @param String $attributeName 移動処理を取得したい属性名
     * @return array 移動処理のレコード
     */

    public static function getMoves(String $attributeName)
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
            ->get();
    }

    /**
     * ID で指定された移動処理を取得する
     * 
     * @param String $attributeName 移動処理を取得したい属性名
     * @param Int 取得する主キー
     * @return array 指定された移動処理のレコード
     */
    public static function getMoveById(String $attributeName, Int $id)
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
            ->get();

        return $list[0];
    }

    /**
     * 移動処理の追加
     * 
     * @param String $attributeName 移動処理を取得したい属性名
     * @param Array $params 登録内容
     *          想定される変数
     *              * item: String 項目
     *              * amount: Int 移動金額
     *              * date: String 移動日の日付
     *              * before_id: Int 移動前の ID
     *              * after_id: Int 移動後の ID
     */
    public static function insertMove(String $attributeName, Request $request)
    {
        $before = array(
            'item' => $request['item'],
            'amount' => (-1) * $request['amount'],
            'kind_element_id' => KindElement::MOVE_ID,
            'date' => $request['date']
        );

        $after = array(
            'item' => $request['item'],
            'amount' => $request['amount'],
            'kind_element_id' => KindElement::MOVE_ID,
            'date' => $request['date']
        );

        if ($attributeName === 'purpose') {
            $before['purpose_element_id'] = $request['before_id'];
            $before['place_element_id'] = PlaceElement::MOVE_ID;
            $after['purpose_element_id'] = $request['after_id'];
            $after['place_element_id'] = PlaceElement::MOVE_ID;
        } else if ($attributeName === 'place') {
            $before['purpose_element_id'] = PurposeElement::MOVE_ID;
            $before['place_element_id'] = $request['before_id'];
            $after['purpose_element_id'] = PurposeElement::MOVE_ID;
            $after['place_element_id'] = $request['after_id'];
        } else {
            // ここにはこない想定
            // exception 吐いたほうがいい
            // もっと上位で処理しても良さそう
        }

        return DB::table('m_balance')->insert([$before, $after]);
    }

    // $request の型が扱いづらいので連想配列で扱えるようにコピー
    public static function insertMoveByArray(String $attributeName, Array $request)
    {
        $before = array(
            'item' => $request['item'],
            'amount' => (-1) * $request['amount'],
            'kind_element_id' => KindElement::MOVE_ID,
            'date' => $request['date']
        );

        $after = array(
            'item' => $request['item'],
            'amount' => $request['amount'],
            'kind_element_id' => KindElement::MOVE_ID,
            'date' => $request['date']
        );

        if ($attributeName === 'purpose') {
            $before['purpose_element_id'] = $request['before_id'];
            $before['place_element_id'] = PlaceElement::MOVE_ID;
            $after['purpose_element_id'] = $request['after_id'];
            $after['place_element_id'] = PlaceElement::MOVE_ID;
        } else if ($attributeName === 'place') {
            $before['purpose_element_id'] = PurposeElement::MOVE_ID;
            $before['place_element_id'] = $request['before_id'];
            $after['purpose_element_id'] = PurposeElement::MOVE_ID;
            $after['place_element_id'] = $request['after_id'];
        } else {
            // ここにはこない想定
            // exception 吐いたほうがいい
            // もっと上位で処理しても良さそう
        }

        return DB::table('m_balance')->insert([$before, $after]);
    }

    /**
     * 移動処理の更新
     * 
     * @param String $attributeName 移動処理を取得したい属性名
     * @param Array $params 登録内容
     *          想定される変数
     *              * item: String 項目
     *              * amount: Int 移動金額
     *              * date: String 移動日の日付
     *              * before_id: Int 移動前の ID
     *              * after_id: Int 移動後の ID
     * @param Int $id 更新したい ID
     */
    public static function updateMove(String $attributeName, Request $request, Int $id)
    {
        $before = array(
            'item' => $request['item'],
            'amount' => (-1) * $request['amount'],
            'date' => $request['date']
        );

        $after = array(
            'item' => $request['item'],
            'amount' => $request['amount'],
            'date' => $request['date']
        );

        if ($attributeName === 'purpose') {
            $before['purpose_element_id'] = $request['before_id'];
            $after['purpose_element_id'] = $request['after_id'];
        } else if ($attributeName === 'place') {
            $before['place_element_id'] = $request['before_id'];
            $after['place_element_id'] = $request['after_id'];
        } else {
            // ここにはこない想定
            // exception 吐いたほうがいい
            // もっと上位で処理しても良さそう
        }

        // 本当に移動処理かどうかのチェックをしたほうがいい
        return DB::transaction(function () use ($id, $before, $after) {
            DB::table('m_balance')->where('id', '=', $id)->update($before);
            DB::table('m_balance')->where('id', '=', $id + 1)->update($after);
        });
    }

    /**
     * ID で指定された移動処理を削除する
     * 
     * @param Int 取得する主キー
     */
    public static function deleteMove(Int $id)
    {
        return DB::table('m_balance')
        ->where('id', '=', $id)
        ->orWhere('id', '=', $id + 1)
        ->delete();
    }
}
