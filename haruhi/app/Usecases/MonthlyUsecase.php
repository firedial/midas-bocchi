<?php

namespace App\Usecases;

use App\Domain\Entities\BalanceEntity;
use App\Domain\Entities\MonthlyPaymentEntity;
use App\Domain\ValueObjects\BalanceId;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Infrastructure\Repository\BalanceRepositoryInterface;
use App\Infrastructure\Repository\Impl\BalanceRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class MonthlyUsecase
{
    private BalanceRepositoryImpl $balanceRepository;

    public function __construct(
        ?BalanceRepositoryInterface $balanceRepository = null,
    ) {
        $this->balanceRepository = $balanceRepository ?: new BalanceRepositoryImpl();
    }

    public function execute(
        MonthlyPaymentEntity $houseRent,
        MonthlyPaymentEntity $gas,
        MonthlyPaymentEntity $water,
        MonthlyPaymentEntity $elect,
        MonthlyPaymentEntity $net,
    ): void {
        $saveBalances = [];

        // 家賃
        $saveBalances[] = $houseRent->shouldSave() ? new BalanceEntity(
            BalanceId::emptyId(),
            $houseRent->amount()->inverse(),
            new Item("家賃"),
            KindElementId::houseId(),
            PurposeElementId::utilCostId(),
            PlaceElementId::withdrawalId(),
            $houseRent->date(),
        ) : null;

        // ガス代
        $saveBalances[] = $gas->shouldSave() ? new BalanceEntity(
            BalanceId::emptyId(),
            $gas->amount()->inverse(),
            new Item("ガス代"),
            KindElementId::gasId(),
            PurposeElementId::utilCostId(),
            PlaceElementId::withdrawalId(),
            $gas->date(),
        ) : null;

        // 水道代
        $saveBalances[] = $water->shouldSave() ? new BalanceEntity(
            BalanceId::emptyId(),
            $water->amount()->inverse(),
            new Item("水道代"),
            KindElementId::waterId(),
            PurposeElementId::utilCostId(),
            PlaceElementId::withdrawalId(),
            $water->date(),
        ) : null;

        // 電気代
        $saveBalances[] = $elect->shouldSave() ? new BalanceEntity(
            BalanceId::emptyId(),
            $elect->amount()->inverse(),
            new Item("電気代"),
            KindElementId::electId(),
            PurposeElementId::utilCostId(),
            PlaceElementId::withdrawalId(),
            $elect->date(),
        ) : null;

        // ネット代
        $saveBalances[] = $net->shouldSave() ? new BalanceEntity(
            BalanceId::emptyId(),
            $net->amount()->inverse(),
            new Item("ネット代"),
            KindElementId::netId(),
            PurposeElementId::utilCostId(),
            PlaceElementId::withdrawalId(),
            $net->date(),
        ) : null;

        DB::beginTransaction();
        try {
            foreach ($saveBalances as $saveBalance) {
                if (is_null($saveBalance)) {
                    continue;
                }
                $this->balanceRepository->insertBalance($saveBalance);
            }

            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }
    }
}
