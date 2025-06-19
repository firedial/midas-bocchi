<?php

namespace App\Domain\ValueObjects;

use App\Exceptions\ValueObjectException;

abstract class Id
{
    private const EMPTY_ID = 0;

    private function __construct(protected readonly int $id) {}

    public static function filledId(int $id): static
    {
        if ($id === self::EMPTY_ID) {
            throw new ValueObjectException("Empty id is used.");
        }
        return new static($id);
    }

    public static function emptyId(): static
    {
        return new static(self::EMPTY_ID);
    }

    public function value(): int
    {
        return $this->id;
    }
}
