<?php

namespace App\Usecases\FixedBalance;

use App\Domain\Entities\FixedBalanceEntity;
use App\Infrastructure\Repository\FixedBalanceRepositoryInterface;
use App\Infrastructure\Repository\Impl\FixedBalanceRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class InsertFixedBalanceUsecase
{
    private FixedBalanceRepositoryImpl $fixedBalanceRepository;

    public function __construct(?FixedBalanceRepositoryInterface $fixedBalanceRepository = null)
    {
        $this->fixedBalanceRepository = $fixedBalanceRepository ?: new FixedBalanceRepositoryImpl();
    }

    public function execute(FixedBalanceEntity $fixedBalance): int
    {
        DB::beginTransaction();
        try {
            // 挿入
            $insertId = $this->fixedBalanceRepository->insertFixedBalance($fixedBalance);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $insertId;
    }
}
