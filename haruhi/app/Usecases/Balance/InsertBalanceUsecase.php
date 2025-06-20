<?php

namespace App\Usecases\Balance;

use App\Domain\Entities\BalanceEntity;
use App\Infrastructure\Repository\BalanceRepositoryInterface;
use App\Infrastructure\Repository\Impl\BalanceRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class InsertBalanceUsecase
{
    private BalanceRepositoryImpl $balanceRepository;

    public function __construct(?BalanceRepositoryInterface $balanceRepository = null)
    {
        $this->balanceRepository = $balanceRepository ?: new BalanceRepositoryImpl();
    }

    public function execute(BalanceEntity $balance): int
    {
        DB::beginTransaction();
        try {
            $insertId = $this->balanceRepository->insertBalance($balance);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $insertId;
    }
}
