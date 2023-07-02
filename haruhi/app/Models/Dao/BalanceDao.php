<?php

namespace App\Models\Dao;

interface BalanceDao
{
    public function selectBalance(array $params);
    public function insertBalance(array $balance);
    public function updateBalance(array $balance);
    public function deleteBalance(int $id);
}
