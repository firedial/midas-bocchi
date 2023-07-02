<?php

namespace App\Service;

use App\Models\Dao\BalanceDao;
use App\Models\Dao\Impl\BalanceDaoImpl;

/**
 * balance テーブルの操作のサービスクラス
 */
class BalanceService
{

    private $balanceDao;

    public function __construct(BalanceDao $balanceDao = null)
    {
        $this->balanceDao = $balanceDao ?: new BalanceDaoImpl();
    }

    public function index(array $params): array
    {
        return $this->balanceDao->selectBalance($params);
    }

    public function show(array $params): array
    {
        return $this->balanceDao->selectBalance($params);
    }

    public function store(array $balance): bool
    {
        return $this->balanceDao->insertBalance($balance);
    }

    public function update(array $balance): bool
    {
        return $this->balanceDao->updateBalance($balance);
    }

    public function destroy(int $id): bool
    {
        return $this->balanceDao->deleteBalance($id);
    }

    public static function deleteBalance(Balance $balance): Bool
    {
        return $balance->delete();
    }
}
