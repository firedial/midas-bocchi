<?php

namespace App\Usecases\Balance;

use App\Domain\ValueObjects\BalanceId;
use App\Exceptions\InternalException;
use App\Exceptions\NotFoundException;
use App\Infrastructure\Repository\BalanceRepositoryInterface;
use App\Infrastructure\Repository\Impl\BalanceRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class DeleteBalanceUsecase
{
    private BalanceRepositoryImpl $balanceRepository;

    public function __construct(?BalanceRepositoryInterface $balanceRepository = null)
    {
        $this->balanceRepository = $balanceRepository ?: new BalanceRepositoryImpl();
    }

    public function execute(BalanceId $balanceId): void
    {
        DB::beginTransaction();
        try {
            $beforeBalance = $this->balanceRepository->selectBalance($balanceId);

            // 存在しないとき
            if (is_null($beforeBalance)) {
                throw new NotFoundException("Not found balance.");
            }

            // 削除
            $this->balanceRepository->deleteBalance($balanceId);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }
    }
}
