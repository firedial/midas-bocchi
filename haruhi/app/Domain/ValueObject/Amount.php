<?php

namespace App\Domain\ValueObject;

class Amount
{
    private int $amount;

    public function __construct(int $amount)
    {
        if ($amount === 0) {
            throw new Exception("wrong amount");
        }

        $this->amount = $amount;
    }

    public function value(): int
    {
        return $this->amount;
    }
}
