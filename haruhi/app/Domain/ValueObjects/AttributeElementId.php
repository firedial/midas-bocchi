<?php

namespace App\Domain\ValueObjects;

class AttributeElementId extends Id
{
    private const MOVE_ID = 1;

    public function isMoveId(): bool
    {
        return $this->id === static::MOVE_ID;
    }

    public static function moveId(): self
    {
        return new self(static::MOVE_ID);
    }
}
