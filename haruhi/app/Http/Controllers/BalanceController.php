<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use App\Models\Balance;
use App\Service\BalanceService;
use App\Models\KindElement;
use App\Models\PurposeElement;
use App\Models\PlaceElement;
use App\Util\DateUtil;
use App\Exceptions\InvalidParameterException;
use App\Exceptions\NotFoundException;

class BalanceController extends Controller
{
    public function index(Request $request)
    {
        $params = [];

        $params['limit'] = $request->input('limit');
        if (!is_null($params['limit']) && !is_numeric($params['limit'])) {
            throw new InvalidParameterException('limit is wrong');
        }
        $params['orderby'] = $request->input('orderby');
        if (!is_null($params['orderby']) && ($params['orderby'] !== 'asc' && $params['orderby'] !== 'desc')) {
            throw new InvalidParameterException('orderby is wrong');
        }
        $params['id'] = null;

        $balanceService = new BalanceService();
        return $balanceService->index($params);
    }

    public function show(int $id)
    {
        // @todo ここら辺セットしなくてもいいようにする
        $params = [];
        $params['limit'] = null;
        $params['orderby'] = null;
        $params['id'] = $id;
        if (is_null($params['id']) || !is_numeric($params['id'])) {
            throw new InvalidParameterException('id is wrong');
        }

        $balanceService = new BalanceService();
        $balances = $balanceService->index($params);
        if (count($balances) === 0) {
            throw new NotFoundException();
        }
        return $balances[0];
    }

    public function store(Request $request)
    {
        $balanceService = new BalanceService();
        $balance = self::getBalance($request);
        self::validation($balance);
        $balanceService->store($balance);
    }

    public function update(Request $request, int $id)
    {
        $balanceService = new BalanceService();
        $balance = self::getBalance($request);
        if (is_null($id) || !is_numeric($id)) {
            throw new InvalidParameterException('Balance id is null.');
        }
        self::validation($balance);
        $balanceService->update($balance);
    }

    public function destroy(int $id)
    {
        $balanceService = new BalanceService();
        if (is_null($id) || !is_numeric($id)) {
            throw new InvalidParameterException('Balance id is null.');
        }
        $balanceService->destroy($id);
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

    private static function validation(array $balance): void
    {
        // 移動処理を表す id が入っていた場合は不正
        if ($balance['kind_element_id'] === KindElement::MOVE_ID) {
            throw new InvalidParameterException('Kind element id is move id.');
        }
        if ($balance['purpose_element_id'] === PurposeElement::MOVE_ID) {
            throw new InvalidParameterException('Purpose element id is move id.');
        }
        if ($balance['place_element_id'] === PlaceElement::MOVE_ID) {
            throw new InvalidParameterException('Place element id is move id.');
        }

        // 金額は 0 でない値じゃないとダメ
        if ($balance['amount'] === 0) {
            throw new InvalidParameterException('Amount is zero.');
        }

        // 項目は空文字ではないとダメ
        if ($balance['item'] === '') {
            throw new InvalidParameterException('Item is empty.');
        }

        // 日付が正しい形式か
        if (!DateUtil::isValidDateString($balance['date'])) {
            throw new InvalidParameterException('Date is invalid.');
        }
    }
}
