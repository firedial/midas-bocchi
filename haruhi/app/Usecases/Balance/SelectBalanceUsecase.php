<?php

namespace App\Usecases\Balance;

use App\Domain\Entities\BalanceEntity;
use App\Domain\ValueObjects\BalanceId;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;
use App\Infrastructure\Repository\BalanceRepositoryInterface;
use App\Infrastructure\Repository\Impl\BalanceRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class SelectBalanceUsecase
{
    private BalanceRepositoryImpl $balanceRepository;

    public function __construct(?BalanceRepositoryInterface $balanceRepository = null)
    {
        $this->balanceRepository = $balanceRepository ?: new BalanceRepositoryImpl();
    }

    public function execute(BalanceId $balanceId): BalanceEntity
    {
        DB::beginTransaction();
        try {
            // 取得
            $balance = $this->balanceRepository->selectBalance($balanceId);

            // 存在しないとき
            if (is_null($balance)) {
                throw new AppException(ErrorCode::RECORD_NOT_FOUND, "Not found balance.");
            }
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $balance;
    }
}
