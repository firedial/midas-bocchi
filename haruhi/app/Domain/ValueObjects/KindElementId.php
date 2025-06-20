<?php

namespace App\Domain\ValueObjects;

class KindElementId extends Id
{
    private const MOVE_ID = 1;

    public function isMoveId(): bool
    {
        return $this->id === self::MOVE_ID;
    }

    public static function moveId(): self
    {
        return new self(self::MOVE_ID);
    }
}
