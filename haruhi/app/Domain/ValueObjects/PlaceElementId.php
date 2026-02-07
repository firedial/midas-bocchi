<?php

namespace App\Domain\ValueObjects;

class PlaceElementId extends AttributeElementId
{
    protected const MOVE_ID = 1;

    private const SKY_PLACE_ELEMENT_ID = 4;
    private const SALARY_PLACE_ELEMENT_ID = 52;
    private const HOLDING_PLACE_ELEMENT_ID = 54;

    public static function skyId(): self
    {
        return new self(self::SKY_PLACE_ELEMENT_ID);
    }

    public static function salaryId(): self
    {
        return new self(self::SALARY_PLACE_ELEMENT_ID);
    }

    public static function holdingId(): self
    {
        return new self(self::HOLDING_PLACE_ELEMENT_ID);
    }
}
