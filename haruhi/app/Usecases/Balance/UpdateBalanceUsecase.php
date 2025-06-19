<?php

namespace App\Usecases\Balance;

use App\Domain\Entities\BalanceEntity;
use App\Exceptions\InternalException;
use App\Exceptions\NotFoundException;
use App\Infrastructure\Repository\BalanceRepositoryInterface;
use App\Infrastructure\Repository\Impl\BalanceRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class UpdateBalanceUsecase
{
    private BalanceRepositoryImpl $balanceRepository;

    public function __construct(BalanceRepositoryInterface $balanceRepository = null)
    {
        $this->balanceRepository = $balanceRepository ?: new BalanceRepositoryImpl();
    }

    public function execute(BalanceEntity $balance): void
    {
        DB::beginTransaction();
        try {
            $beforeBalance = $this->balanceRepository->selectBalance($balance->balanceId());
            if (is_null($beforeBalance)) {
                throw new NotFoundException("Not found balance.");
            }

            if ($beforeBalance->isMove()) {
                throw new InternalException("Can not update move record.");
            }
            $this->balanceRepository->updateBalance($balance);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }
    }
}
