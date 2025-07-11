<?php

namespace App\Domain\ValueObjects;

use App\Exceptions\ValueObjectException;

class Priority
{
    public function __construct(private readonly int $priority)
    {
        if ($priority < 0 || $priority > 100) {
            throw new ValueObjectException("Priority is wrong.");
        }
    }

    public function value(): int
    {
        return $this->priority;
    }
}
