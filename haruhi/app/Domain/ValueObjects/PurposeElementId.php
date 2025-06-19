<?php

namespace App\Domain\ValueObjects;

class PurposeElementId extends Id
{
    private const MOVE_ID = 1;

    public function isMoveId(): bool
    {
        return $this->id === self::MOVE_ID;
    }
}
