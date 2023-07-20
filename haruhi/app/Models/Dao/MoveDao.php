<?php

namespace App\Models\Dao;

use Illuminate\Http\Request;

interface MoveDao
{
    public function getMoves(String $attributeName);
    public function getMoveById(String $attributeName, Int $id);
}
