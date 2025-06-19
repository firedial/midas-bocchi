<?php

namespace App\Domain\ValueObjects;

class Amount
{
    public function __construct(private readonly int $amount) {}

    public function value(): int
    {
        return $this->amount;
    }
}
