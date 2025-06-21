<?php

namespace App\Domain\Entities;

use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Date;

class BonusEntity
{
    public function __construct(
        protected readonly Amount $bonus,
        protected readonly Amount $healthInsurance,
        protected readonly Amount $welfarePension,
        protected readonly Amount $employmentInsurance,
        protected readonly Amount $incomeTax,
        protected readonly Date $date,
    ) {}

    public function bonus(): Amount
    {
        return $this->bonus;
    }

    public function healthInsurance(): Amount
    {
        return $this->healthInsurance;
    }

    public function welfarePension(): Amount
    {
        return $this->welfarePension;
    }

    public function employmentInsurance(): Amount
    {
        return $this->employmentInsurance;
    }

    public function incomeTax(): Amount
    {
        return $this->incomeTax;
    }

    public function date(): Date
    {
        return $this->date;
    }
}
