<?php

namespace App\Domain\Entities;

use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Date;
use App\Exceptions\ValueObjectException;

class MonthlyPaymentEntity
{
    private readonly Amount $amount;
    private readonly ?Date $date;

    public function __construct(
        protected readonly array $data,
    ) {
        $this->amount = new Amount($this->data['amount']);

        if ($this->amount->value() < 0) {
            throw new ValueObjectException("Amount is minus.");
        } else if ($this->amount->value() === 0) {
            $this->date = null;
        } else {
            $this->date = new Date($this->data['date']);
        }
    }

    public function shouldSave(): bool
    {
        return $this->amount->value() > 0;
    }

    public function amount(): Amount
    {
        return $this->amount;
    }

    public function date(): ?Date
    {
        return $this->date;
    }
}
