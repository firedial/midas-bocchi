<?php

namespace App\Usecases\FixedBalance;

use App\Infrastructure\Repository\FixedBalanceRepositoryInterface;
use App\Infrastructure\Repository\Impl\FixedBalanceRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class GetFixedBalancesUsecase
{
    private FixedBalanceRepositoryImpl $fixedBalanceRepository;

    public function __construct(?FixedBalanceRepositoryInterface $fixedBalanceRepository = null)
    {
        $this->fixedBalanceRepository = $fixedBalanceRepository ?: new FixedBalanceRepositoryImpl();
    }

    public function execute(): array
    {
        DB::beginTransaction();
        try {
            // 移動レコードではないものを取得
            $fixedBalances = $this->fixedBalanceRepository->getFixedBalances();
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $fixedBalances;
    }
}
