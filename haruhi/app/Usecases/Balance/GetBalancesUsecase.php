<?php

namespace App\Usecases\Balance;

use App\Infrastructure\Repository\BalanceRepositoryInterface;
use App\Infrastructure\Repository\Impl\BalanceRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class GetBalancesUsecase
{
    private BalanceRepositoryImpl $balanceRepository;

    public function __construct(?BalanceRepositoryInterface $balanceRepository = null)
    {
        $this->balanceRepository = $balanceRepository ?: new BalanceRepositoryImpl();
    }

    public function execute(?int $limit, ?bool $orderByDesc): array
    {
        DB::beginTransaction();
        try {
            // 移動レコードではないものを取得
            $balances = $this->balanceRepository->getBalances(limit: $limit, orderByDesc: $orderByDesc);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $balances;
    }
}
