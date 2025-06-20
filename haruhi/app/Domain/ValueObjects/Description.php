<?php

namespace App\Domain\ValueObjects;

use App\Exceptions\ValueObjectException;

class Description
{
    private const MAX_LENGTH = 20;

    public function __construct(private readonly string $description)
    {
        if (mb_strlen($description) === 0 || mb_strlen($description) > self::MAX_LENGTH) {
            throw new ValueObjectException("Item length is over.");
        }
    }

    public function value(): string
    {
        return $this->description;
    }
}
