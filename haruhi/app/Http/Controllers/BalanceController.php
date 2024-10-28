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

class BalanceController extends Controller
{
    public function index(Request $request)
    {
        $params = [];
        $params['limit'] = $request->input('limit');
        $params['orderby'] = $request->input('orderby');
        $params['id'] = null;

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
        $balance = self::getBalance($request);
        self::validation($balance);
        return $balanceService->store($balance);
    }

    public function update(Request $request)
    {
        $balanceService = new BalanceService();
        $balance = self::getBalance($request);
        if (is_null($balance['id'])) {
            throw new InvalidParameterException('Balance id is null.');
        }
        self::validation($balance);
        return $balanceService->update($balance);
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
