<?php

namespace App\Usecases\FixedBalance;

use App\Domain\ValueObjects\FixedBalanceId;
use App\Exceptions\NotFoundException;
use App\Infrastructure\Repository\FixedBalanceRepositoryInterface;
use App\Infrastructure\Repository\Impl\FixedBalanceRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class DeleteFixedBalanceUsecase
{
    private FixedBalanceRepositoryImpl $fixedBalanceRepository;

    public function __construct(?FixedBalanceRepositoryInterface $fixedBalanceRepository = null)
    {
        $this->fixedBalanceRepository = $fixedBalanceRepository ?: new FixedBalanceRepositoryImpl();
    }

    public function execute(FixedBalanceId $fixedBalanceId): void
    {
        DB::beginTransaction();
        try {
            $beforeFixedBalance = $this->fixedBalanceRepository->selectFixedBalance($fixedBalanceId);

            // 存在しないとき
            if (is_null($beforeFixedBalance)) {
                throw new NotFoundException("Not found fixed balance.");
            }

            // 削除
            $this->fixedBalanceRepository->deleteFixedBalance($fixedBalanceId);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }
    }
}
