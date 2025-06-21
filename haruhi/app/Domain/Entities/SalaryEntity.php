<?php

namespace App\Domain\Entities;

use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Date;

class SalaryEntity
{
    public function __construct(
        protected readonly Amount $baseSalary,
        protected readonly Amount $adjustmentSalary,
        protected readonly Amount $transportation,
        protected readonly Amount $holdingIncentives,
        protected readonly Amount $healthInsurance,
        protected readonly Amount $welfarePension,
        protected readonly Amount $residentTax,
        protected readonly Amount $employmentInsurance,
        protected readonly Amount $incomeTax,
        protected readonly Amount $holding,
        protected readonly Date $date,
    ) {}

    public function baseSalary(): Amount
    {
        return $this->baseSalary;
    }

    public function adjustmentSalary(): Amount
    {
        return $this->adjustmentSalary;
    }

    public function transportation(): Amount
    {
        return $this->transportation;
    }

    public function holdingIncentives(): Amount
    {
        return $this->holdingIncentives;
    }

    public function healthInsurance(): Amount
    {
        return $this->healthInsurance;
    }

    public function welfarePension(): Amount
    {
        return $this->welfarePension;
    }

    public function residentTax(): Amount
    {
        return $this->residentTax;
    }

    public function employmentInsurance(): Amount
    {
        return $this->employmentInsurance;
    }

    public function incomeTax(): Amount
    {
        return $this->incomeTax;
    }

    public function holding(): Amount
    {
        return $this->holding;
    }

    public function date(): Date
    {
        return $this->date;
    }
}
