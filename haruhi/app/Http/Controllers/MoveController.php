<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use App\Service\MoveService;
use App\Exceptions\InvalidParameterException;
use App\Util\DateUtil;

class MoveController extends Controller
{
    // @todo 入力値をそのまま入れているのでバリデーションを入れる
    public function index(String $attributeName)
    {
        $moveService = new MoveService($attributeName);
        return $moveService->index();
    }

    public function show(String $attributeName, Int $id)
    {
        $moveService = new MoveService($attributeName);
        return $moveService->show($id);
    }

    public function store(Request $request, String $attributeName)
    {
        $move = [];
        $move['item'] = (string)$request->input('item');
        $move['amount'] = (int)$request->input('amount');
        $move['date'] = (string)$request->input('date');
        $move['beforeId'] = (int)$request->input('before_id');
        $move['afterId'] = (int)$request->input('after_id');

        self::validation($move);
        $moveService = new MoveService($attributeName);
        return $moveService->store($move);
    }

    public function update(Request $request, String $attributeName, Int $id)
    {
        $move = [];
        $move['item'] = (string)$request->input('item');
        $move['amount'] = (int)$request->input('amount');
        $move['date'] = (string)$request->input('date');
        $move['beforeId'] = (int)$request->input('before_id');
        $move['afterId'] = (int)$request->input('after_id');

        if (is_null($id)) {
            throw new InvalidParameterException('Move id is null.');
        }

        self::validation($move);
        $moveService = new MoveService($attributeName);
        return $moveService->update($move, $id);
    }

    public function destroy(String $attributeName, Int $id)
    {
        $moveService = new MoveService($attributeName);
        return $moveService->destroy($id);
    }

    private static function validation(array $move): void
    {
        // 金額は 1 以上じゃないとダメ
        if ($move['amount'] <= 0) {
            throw new InvalidParameterException('Amount is zero or minus.');
        }

        // 項目は空文字ではないとダメ
        if ($move['item'] === '') {
            throw new InvalidParameterException('Item is empty.');
        }

        // 移動していないとダメ
        if ($move['beforeId'] === $move['afterId']) {
            throw new InvalidParameterException('Before id and after id is the same.');
        }

        // 日付が正しい形式か
        if (!DateUtil::isValidDateString($move['date'])) {
            throw new InvalidParameterException('Date is invalid.');
        }
    }
}
