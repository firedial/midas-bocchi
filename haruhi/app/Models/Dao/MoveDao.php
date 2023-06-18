<?php

namespace App\Models\Dao;

use Illuminate\Http\Request;

interface MoveDao
{
    public function getMoves(String $attributeName);
    public function getMoveById(String $attributeName, Int $id);
    public function insertMove(String $attributeName, Request $request);
    public function insertMoveByArray(String $attributeName, Array $request);
    public function updateMove(String $attributeName, Request $request, Int $id);
    public function deleteMove(Int $id);
}
