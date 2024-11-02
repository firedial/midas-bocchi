<?php

namespace App\Models\Dao;

use Illuminate\Http\Request;

interface MoveDao
{
    public function getMoves(string $attributeName);
    public function getMoveById(string $attributeName, int $id);
    public function insertMove(string $attributeName, array $move);
}
