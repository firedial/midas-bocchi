<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use App\Service\MoveService;
use App\Exceptions\InvalidParameterException;
use App\Exceptions\NotFoundException;
use App\Util\DateUtil;

class MoveController extends Controller
{
    private const MOVE_ID = 1;

    // @todo 入力値をそのまま入れているのでバリデーションを入れる
    public function index(string $attributeName)
    {
        // 要素名が想定内かどうか
        if ($attributeName !== 'purposes' && $attributeName !== 'places') {
            throw new InvalidParameterException('Attribute name is wrong.');
        }

        $moveService = new MoveService($attributeName);
        return $moveService->index();
    }

    public function show(string $attributeName, int $id)
    {
        // 要素名が想定内かどうか
        if ($attributeName !== 'purposes' && $attributeName !== 'places') {
            throw new InvalidParameterException('Attribute name is wrong.');
        }

        if (is_null($id) || !is_numeric($id)) {
            throw new InvalidParameterException('Move id is null.');
        }

        $moveService = new MoveService($attributeName);
        $moves = $moveService->show($id);

        if (count($moves) === 0) {
            throw new NotFoundException();
        }

        return $moves[0];
    }

    public function store(Request $request, string $attributeName)
    {
        $move = [];
        $move['item'] = (string)$request->input('item');
        $move['amount'] = (int)$request->input('amount');
        $move['date'] = (string)$request->input('date');
        $move['beforeId'] = (int)$request->input('before_id');
        $move['afterId'] = (int)$request->input('after_id');

        self::validation($move);
        $moveService = new MoveService($attributeName);
        $id = $moveService->store($move);
        return ['id' => $id];
    }

    public function update(Request $request, string $attributeName, int $id)
    {
        // 要素名が想定内かどうか
        if ($attributeName !== 'purposes' && $attributeName !== 'places') {
            throw new InvalidParameterException('Attribute name is wrong.');
        }

        $move = [];
        $move['item'] = (string)$request->input('item');
        $move['amount'] = (int)$request->input('amount');
        $move['date'] = (string)$request->input('date');
        $move['beforeId'] = (int)$request->input('before_id');
        $move['afterId'] = (int)$request->input('after_id');

        if (is_null($id) || !is_numeric($id)) {
            throw new InvalidParameterException('Move id is null.');
        }

        self::validation($move);
        $moveService = new MoveService($attributeName);

        $moves = $moveService->show($id);
        if (count($moves) === 0) {
            throw new NotFoundException();
        }

        $moveService->update($move, $id);
    }

    public function destroy(string $attributeName, int $id)
    {
        // 要素名が想定内かどうか
        if ($attributeName !== 'purposes' && $attributeName !== 'places') {
            throw new InvalidParameterException('Attribute name is wrong.');
        }

        $moveService = new MoveService($attributeName);

        $moves = $moveService->show($id);
        if (count($moves) === 0) {
            throw new NotFoundException();
        }
        $moveService->destroy($id);
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

        // 入力されていないとダメ
        if ($move['beforeId'] === 0 || $move['afterId'] === 0) {
            throw new InvalidParameterException('Before id and after id is null.');
        }

        // 移動前後で移動IDを使っちゃダメ
        // @todo purpose, place のモデルの定数に変える
        if ($move['beforeId'] === self::MOVE_ID || $move['afterId'] === self::MOVE_ID) {
            throw new InvalidParameterException('Before id or after id is move id.');
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
