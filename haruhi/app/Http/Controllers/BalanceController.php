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
        $params = [];
        $params['limit'] = $request->input('limit');
        $params['orderby'] = $request->input('orderby');
        $params['id'] = $request->input('id');

        $balanceService = new BalanceService();
        return $balanceService->index($params);
    }

    public function show(Balance $balance)
    {
        // @todo ここら辺セットしなくてもいいようにする
        $params = [];
        $params['limit'] = null;
        $params['orderby'] = null;
        $params['id'] = $balance['id'];

        $balanceService = new BalanceService();
        // @todo あるかどうか判定する
        return $balanceService->index($params)[0];
    }

    public function store(Request $request)
    {
        $balanceService = new BalanceService();
        return $balanceService->store(self::getBalance($request));
    }

    public function update(Request $request)
    {
        $balanceService = new BalanceService();
        return $balanceService->update(self::getBalance($request));
    }

    public function destroy(Balance $balance): bool
    {
        $balanceService = new BalanceService();
        return $balanceService->destroy($balance['id']);
    }

    private static function getBalance(Request $request): array
    {
        $balance = [];
        $balance['id'] = $request->input('id') === null ? null : (int)$request->input('id');
        $balance['amount'] = (int)$request->input('amount');
        $balance['item'] = (string)$request->input('item');
        $balance['kind_element_id'] = (int)$request->input('kind_element_id');
        $balance['purpose_element_id'] = (int)$request->input('purpose_element_id');
        $balance['place_element_id'] = (int)$request->input('place_element_id');
        $balance['date'] = (string)$request->input('date');
        return $balance;
    }
}
