<?php

namespace App\Usecases;

use App\Domain\Entities\BalanceEntity;
use App\Domain\Entities\BonusEntity;
use App\Domain\Entities\MoveEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\BalanceId;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\MoveId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Infrastructure\Repository\BalanceRepositoryInterface;
use App\Infrastructure\Repository\Impl\BalanceRepositoryImpl;
use App\Infrastructure\Repository\Impl\MoveRepositoryImpl;
use App\Infrastructure\Repository\MoveRepositoryInterface;
use Exception;
use Illuminate\Support\Facades\DB;

class BonusUsecase
{
    private BalanceRepositoryImpl $balanceRepository;
    private MoveRepositoryImpl $moveRepository;

    public function __construct(
        ?BalanceRepositoryInterface $balanceRepository = null,
        ?MoveRepositoryInterface $moveRepository = null,
    ) {
        $this->balanceRepository = $balanceRepository ?: new BalanceRepositoryImpl();
        $this->moveRepository = $moveRepository ?: new MoveRepositoryImpl();
    }

    public function execute(BonusEntity $bonus): void
    {
        $baseBonus = new BalanceEntity(
            BalanceId::emptyId(),
            $bonus->bonus(),
            new Item("ボーナス"),
            KindElementId::salaryId(),
            PurposeElementId::incomeId(),
            PlaceElementId::skyId(),
            $bonus->date(),
        );

        $deductionAmount = new Amount(
            $bonus->healthInsurance()->value()
                + $bonus->welfarePension()->value()
                + $bonus->employmentInsurance()->value()
                + $bonus->incomeTax()->value()
        );

        $deductionMove = new MoveEntity(
            MoveId::emptyId(),
            $deductionAmount,
            new Item("予算移動"),
            PurposeElementId::incomeId(),
            PurposeElementId::deductionId(),
            $bonus->date(),
        );

        $healthInsurance = new BalanceEntity(
            BalanceId::emptyId(),
            $bonus->healthInsurance()->inverse(),
            new Item("健康保険料"),
            KindElementId::deductionId(),
            PurposeElementId::deductionId(),
            PlaceElementId::skyId(),
            $bonus->date(),
        );

        $welfarePension = new BalanceEntity(
            BalanceId::emptyId(),
            $bonus->welfarePension()->inverse(),
            new Item("厚生年金保険"),
            KindElementId::deductionId(),
            PurposeElementId::deductionId(),
            PlaceElementId::skyId(),
            $bonus->date(),
        );

        $employmentInsurance = new BalanceEntity(
            BalanceId::emptyId(),
            $bonus->employmentInsurance()->inverse(),
            new Item("雇用保険料"),
            KindElementId::deductionId(),
            PurposeElementId::deductionId(),
            PlaceElementId::skyId(),
            $bonus->date(),
        );

        $incomeTax = new BalanceEntity(
            BalanceId::emptyId(),
            $bonus->incomeTax()->inverse(),
            new Item("所得税"),
            KindElementId::deductionId(),
            PurposeElementId::deductionId(),
            PlaceElementId::skyId(),
            $bonus->date(),
        );

        $takeBonus = new Amount(
            $bonus->bonus()->value() - $deductionAmount->value()
        );

        $mainMove = new MoveEntity(
            MoveId::emptyId(),
            $takeBonus,
            new Item("場所移動"),
            PlaceElementId::skyId(),
            PlaceElementId::salaryId(),
            $bonus->date(),
        );

        DB::beginTransaction();
        try {
            $this->balanceRepository->insertBalance($baseBonus);

            $this->moveRepository->insertMove(Attribute::purpose(), $deductionMove);

            $this->balanceRepository->insertBalance($healthInsurance);
            $this->balanceRepository->insertBalance($welfarePension);
            $this->balanceRepository->insertBalance($employmentInsurance);
            $this->balanceRepository->insertBalance($incomeTax);

            $this->moveRepository->insertMove(Attribute::place(), $mainMove);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }
    }
}
