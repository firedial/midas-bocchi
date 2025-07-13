<?php

namespace App\Domain\ValueObjects;

use App\Exceptions\ValueObjectException;

class Item
{
    private const MAX_LENGTH = 50;

    public function __construct(private readonly string $item)
    {
        if (mb_strlen($item) === 0 || mb_strlen($item) > self::MAX_LENGTH) {
            throw new ValueObjectException("Item length is over.");
        }
    }

    public function value(): string
    {
        return $this->item;
    }
}
