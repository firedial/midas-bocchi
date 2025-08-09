<?php

namespace App\Usecases\FixedBalance;

use App\Domain\ValueObjects\FixedBalanceId;
use App\Domain\Entities\FixedBalanceEntity;
use App\Exceptions\NotFoundException;
use App\Infrastructure\Repository\FixedBalanceRepositoryInterface;
use App\Infrastructure\Repository\Impl\FixedBalanceRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class SelectFixedBalanceUsecase
{
    private FixedBalanceRepositoryImpl $fixedBalanceRepository;

    public function __construct(?FixedBalanceRepositoryInterface $fixedBalanceRepository = null)
    {
        $this->fixedBalanceRepository = $fixedBalanceRepository ?: new FixedBalanceRepositoryImpl();
    }

    public function execute(FixedBalanceId $fixedBalanceId): FixedBalanceEntity
    {
        DB::beginTransaction();
        try {
            // 取得
            $fixedBalance = $this->fixedBalanceRepository->selectFixedBalance($fixedBalanceId);

            // 存在しないとき
            if (is_null($fixedBalance)) {
                throw new NotFoundException("Not found fixed balance.");
            }
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $fixedBalance;
    }
}
