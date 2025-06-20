<?php

namespace App\Usecases\Balance;

use App\Domain\Entities\BalanceEntity;
use App\Domain\ValueObjects\BalanceId;
use App\Domain\ValueObjects\KindElementId;
use App\Exceptions\NotFoundException;
use App\Infrastructure\Repository\BalanceRepositoryInterface;
use App\Infrastructure\Repository\Impl\BalanceRepositoryImpl;
use App\Models\KindElement;
use Exception;
use Illuminate\Support\Facades\DB;

class GetBalancesUsecase
{
    private BalanceRepositoryImpl $balanceRepository;

    public function __construct(?BalanceRepositoryInterface $balanceRepository = null)
    {
        $this->balanceRepository = $balanceRepository ?: new BalanceRepositoryImpl();
    }

    public function execute(?int $limit, ?bool $orderByDesc): array
    {
        DB::beginTransaction();
        try {
            $balances = $this->balanceRepository->getBalances(notKindElementId: KindElementId::moveId(), limit: $limit, orderByDesc: $orderByDesc);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $balances;
    }
}
