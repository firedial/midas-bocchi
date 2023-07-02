<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use App\Service\MoveService;
use App\Models\Dao\MoveDao;

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
        $move['item'] = $request->input('item');
        $move['amount'] = $request->input('amount');
        $move['date'] = $request->input('date');
        $move['beforeId'] = $request->input('before_id');
        $move['afterId'] = $request->input('after_id');

        $moveService = new MoveService($attributeName);
        return $moveService->store($move);
    }

    public function update(Request $request, String $attributeName, Int $id)
    {
        $move = [];
        $move['item'] = $request->input('item');
        $move['amount'] = $request->input('amount');
        $move['date'] = $request->input('date');
        $move['beforeId'] = $request->input('before_id');
        $move['afterId'] = $request->input('after_id');

        $moveService = new MoveService($attributeName);
        return $moveService->update($move, $id);
        return MoveDao::updateMove(self::getTableName($attributeName), $request, $id);
    }

    public function destroy(String $attributeName, Int $id)
    {
        $moveService = new MoveService($attributeName);
        return $moveService->destroy($id);
    }
}
