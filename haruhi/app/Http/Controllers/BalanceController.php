<?php

namespace App\Http\Controllers;

use Illuminate\Support\Facades\DB;
use Illuminate\Http\Request;
use App\Models\Balance;
use App\Models\KindElement;
use App\Service\BalanceService;

class BalanceController extends Controller
{
    public function index(Request $request)
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
        if (is_numeric($request->input('limit'))) {
            $query->limit($request->input('limit'));
        }
        // @todo クエリそのまま入れているので後で修正する
        if (!is_null($request->input('orderby'))) {
            $query->orderby('m_balance.id', $request->input('orderby'));
        }

        return $query->get();
    }

    public function show(Balance $balance)
    {
        return $balance;
    }

    public function store(Request $request)
    {
        return BalanceService::createBalance(self::getBalance($request));
    }

    public function update(Request $request, Balance $balance)
    {
        $balance->update($request->all());
        return $balance;
    }

    public function destroy(Balance $balance): boolean
    {
        return BalanceService::deleteBalance($balance);
    }

    private static function getBalance(Request $request): Balance
    {
        $balance = new Balance();
        $balance->id = $request->id === null ? null : (int)$request->id;
        $balance->amount = (int)$request->amount;
        $balance->item = (string)$request->item;
        $balance->kind_element_id = (int)$request->kind_element_id;
        $balance->purpose_element_id = (int)$request->purpose_element_id;
        $balance->place_element_id = (int)$request->place_element_id;
        $balance->date = (string)$request->date;
        return $balance;
    }
}
