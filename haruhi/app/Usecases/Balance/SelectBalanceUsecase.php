<?php

namespace App\Usecases\Balance;

use App\Domain\Entities\BalanceEntity;
use App\Domain\ValueObjects\BalanceId;
use App\Exceptions\NotFoundException;
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
            $balance = $this->balanceRepository->selectBalance($balanceId);
            if (is_null($balance)) {
                throw new NotFoundException("Not found balance.");
            }
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $balance;
    }
}
