<?php

namespace App\Usecases\FixedBalance;

use App\Domain\Entities\FixedBalanceEntity;
use App\Exceptions\NotFoundException;
use App\Infrastructure\Repository\FixedBalanceRepositoryInterface;
use App\Infrastructure\Repository\Impl\FixedBalanceRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class UpdateFixedBalanceUsecase
{
    private FixedBalanceRepositoryImpl $fixedBalanceRepository;

    public function __construct(?FixedBalanceRepositoryInterface $fixedBalanceRepository = null)
    {
        $this->fixedBalanceRepository = $fixedBalanceRepository ?: new FixedBalanceRepositoryImpl();
    }

    public function execute(FixedBalanceEntity $fixedBalance): void
    {
        DB::beginTransaction();
        try {
            $beforeFixedBalance = $this->fixedBalanceRepository->selectFixedBalance($fixedBalance->fixedBalanceId());

            // 存在しないとき
            if (is_null($beforeFixedBalance)) {
                throw new NotFoundException("Not found fixed balance.");
            }

            // 更新
            $this->fixedBalanceRepository->updateFixedBalance($fixedBalance);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }
    }
}
