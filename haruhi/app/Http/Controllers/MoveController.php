<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use App\Models\Dao\MoveDao;

class MoveController extends Controller
{
    // @todo 入力値をそのまま入れているのでバリデーションを入れる
    public function index(String $attributeName)
    {
        return MoveDao::getMoves(self::getTableName($attributeName));
    }

    public function show(String $attributeName, Int $id)
    {
        return MoveDao::getMoveById(self::getTableName($attributeName), $id);
    }

    public function store(Request $request, String $attributeName)
    {
        return MoveDao::insertMove(self::getTableName($attributeName), $request);
    }

    public function update(Request $request, String $attributeName, Int $id)
    {
        return MoveDao::updateMove(self::getTableName($attributeName), $request, $id);
    }

    public function destroy(String $attributeName, Int $id)
    {
        // Note: $attributeName は処理に使っていない
        return MoveDao::deleteMove($id);
    }


    private static function getTableName($attributeName)
    {
        if ($attributeName === 'purposes') {
            return 'purpose';
        }
        if ($attributeName === 'places') {
            return 'place';
        }

        // ここにはこない想定
        return '';
    }
}
