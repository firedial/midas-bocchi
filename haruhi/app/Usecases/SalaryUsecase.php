<?php

namespace App\Usecases;

use App\Domain\Entities\BalanceEntity;
use App\Domain\Entities\MoveEntity;
use App\Domain\Entities\SalaryEntity;
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

class SalaryUsecase
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

    public function execute(SalaryEntity $salary): void
    {
        $baseSalary = new BalanceEntity(
            BalanceId::emptyId(),
            $salary->baseSalary(),
            new Item("基本給"),
            KindElementId::salaryId(),
            PurposeElementId::incomeId(),
            PlaceElementId::skyId(),
            $salary->date(),
        );

        $adjustmentSalary = new BalanceEntity(
            BalanceId::emptyId(),
            $salary->adjustmentSalary(),
            new Item("職務調整給"),
            KindElementId::salaryId(),
            PurposeElementId::incomeId(),
            PlaceElementId::skyId(),
            $salary->date(),
        );

        $transportation = new BalanceEntity(
            BalanceId::emptyId(),
            $salary->transportation(),
            new Item("非課税通勤費"),
            KindElementId::transportationId(),
            PurposeElementId::incomeId(),
            PlaceElementId::skyId(),
            $salary->date(),
        );

        $holdingIncentives = new BalanceEntity(
            BalanceId::emptyId(),
            $salary->holdingIncentives(),
            new Item("持株奨励金"),
            KindElementId::holdingId(),
            PurposeElementId::incomeId(),
            PlaceElementId::skyId(),
            $salary->date(),
        );

        $transportationMove = new MoveEntity(
            MoveId::emptyId(),
            $salary->transportation(),
            new Item("予算移動"),
            PurposeElementId::incomeId(),
            PurposeElementId::transportationId(),
            $salary->date(),
        );

        $deductionAmount = new Amount(
            $salary->healthInsurance()->value()
                + $salary->welfarePension()->value()
                + $salary->residentTax()->value()
                + $salary->employmentInsurance()->value()
                + $salary->incomeTax()->value()
                + $salary->holding()->value()
        );

        $deductionMove = new MoveEntity(
            MoveId::emptyId(),
            $deductionAmount,
            new Item("予算移動"),
            PurposeElementId::incomeId(),
            PurposeElementId::deductionId(),
            $salary->date(),
        );

        $healthInsurance = new BalanceEntity(
            BalanceId::emptyId(),
            $salary->healthInsurance()->inverse(),
            new Item("健康保険料"),
            KindElementId::deductionId(),
            PurposeElementId::deductionId(),
            PlaceElementId::skyId(),
            $salary->date(),
        );

        $welfarePension = new BalanceEntity(
            BalanceId::emptyId(),
            $salary->welfarePension()->inverse(),
            new Item("厚生年金保険"),
            KindElementId::deductionId(),
            PurposeElementId::deductionId(),
            PlaceElementId::skyId(),
            $salary->date(),
        );

        $residentTax = new BalanceEntity(
            BalanceId::emptyId(),
            $salary->residentTax()->inverse(),
            new Item("住民税"),
            KindElementId::deductionId(),
            PurposeElementId::deductionId(),
            PlaceElementId::skyId(),
            $salary->date(),
        );

        $employmentInsurance = new BalanceEntity(
            BalanceId::emptyId(),
            $salary->employmentInsurance()->inverse(),
            new Item("雇用保険料"),
            KindElementId::deductionId(),
            PurposeElementId::deductionId(),
            PlaceElementId::skyId(),
            $salary->date(),
        );

        $incomeTax = new BalanceEntity(
            BalanceId::emptyId(),
            $salary->incomeTax()->inverse(),
            new Item("所得税"),
            KindElementId::deductionId(),
            PurposeElementId::deductionId(),
            PlaceElementId::skyId(),
            $salary->date(),
        );

        $holding = new BalanceEntity(
            BalanceId::emptyId(),
            $salary->holding()->inverse(),
            new Item("持株"),
            KindElementId::shareHeldId(),
            PurposeElementId::deductionId(),
            PlaceElementId::skyId(),
            $salary->date(),
        );

        $takeSalary = new Amount(
            $salary->baseSalary()->value()
                + $salary->adjustmentSalary()->value()
                + $salary->transportation()->value()
                + $salary->holdingIncentives()->value()
                - $deductionAmount->value()
        );

        $mainMove = new MoveEntity(
            MoveId::emptyId(),
            $takeSalary,
            new Item("場所移動"),
            PlaceElementId::skyId(),
            PlaceElementId::salaryId(),
            $salary->date(),
        );

        DB::beginTransaction();
        try {
            $this->balanceRepository->insertBalance($baseSalary);
            $this->balanceRepository->insertBalance($adjustmentSalary);
            $this->balanceRepository->insertBalance($transportation);
            $this->balanceRepository->insertBalance($holdingIncentives);

            $this->moveRepository->insertMove(Attribute::purpose(), $transportationMove);
            $this->moveRepository->insertMove(Attribute::purpose(), $deductionMove);

            $this->balanceRepository->insertBalance($healthInsurance);
            $this->balanceRepository->insertBalance($welfarePension);
            $this->balanceRepository->insertBalance($residentTax);
            $this->balanceRepository->insertBalance($employmentInsurance);
            $this->balanceRepository->insertBalance($incomeTax);
            $this->balanceRepository->insertBalance($holding);

            $this->moveRepository->insertMove(Attribute::place(), $mainMove);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }
    }
}
