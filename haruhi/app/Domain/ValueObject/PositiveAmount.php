<?php

namespace App\Domain\ValueObject;

class PositiveAmount
{
    private int $amount;

    public function __construct(int $amount)
    {
        if ($amount <= 0) {
            throw new Exception("negative number");
        }

        $this->amount = $amount;
    }

    public function value(): int
    {
        return $this->amount;
    }
}
