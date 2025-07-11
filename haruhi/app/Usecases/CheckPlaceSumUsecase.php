<?php

namespace App\Usecases;

use App\Domain\ValueObjects\Date;
use App\Domain\ValueObjects\PlaceElementId;
use App\Infrastructure\Repository\BalanceRepositoryInterface;
use App\Infrastructure\Repository\CheckPlaceSumRepositoryInterface;
use App\Infrastructure\Repository\Impl\BalanceRepositoryImpl;
use App\Infrastructure\Repository\Impl\CheckPlaceSumRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class CheckPlaceSumUsecase
{
    private BalanceRepositoryImpl $balanceRepository;
    private CheckPlaceSumRepositoryImpl $checkPlaceSumRepository;

    public function __construct(
        ?BalanceRepositoryInterface $balanceRepository = null,
        ?CheckPlaceSumRepositoryInterface $checkPlaceSumRepository = null,
    ) {
        $this->balanceRepository = $balanceRepository ?: new BalanceRepositoryImpl();
        $this->checkPlaceSumRepository = $checkPlaceSumRepository ?: new CheckPlaceSumRepositoryImpl();
    }

    public function execute(PlaceElementId $placeElementId, Date $date): void
    {
        DB::beginTransaction();
        try {
            $sum = $this->balanceRepository->sum($placeElementId);

            $this->checkPlaceSumRepository->insertCheckPlaceSum(
                $sum,
                $placeElementId,
                $date,
            );
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }
    }
}
