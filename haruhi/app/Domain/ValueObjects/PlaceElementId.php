<?php

namespace App\Domain\ValueObjects;

class PlaceElementId extends AttributeElementId
{
    protected const MOVE_ID = 1;

    private const WITHDRAWAL_PLACE_ELEMENT_ID = 9;

    private const SKY_PLACE_ELEMENT_ID = 4;
    private const SALARY_PLACE_ELEMENT_ID = 8;

    public static function withdrawalId(): self
    {
        return new self(self::WITHDRAWAL_PLACE_ELEMENT_ID);
    }

    public static function skyId(): self
    {
        return new self(self::SKY_PLACE_ELEMENT_ID);
    }

    public static function salaryId(): self
    {
        return new self(self::SALARY_PLACE_ELEMENT_ID);
    }
}
