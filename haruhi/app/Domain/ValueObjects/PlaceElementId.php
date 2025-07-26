<?php

namespace App\Domain\ValueObjects;

class PlaceElementId extends AttributeElementId
{
    protected const MOVE_ID = 1;

    private const WITHDRAWAL_PLACE_ELEMENT_ID = 9;
    private const PAY_CARD_PLACE_ELEMENT_ID = 48;

    private const SKY_PLACE_ELEMENT_ID = 4;
    private const SALARY_PLACE_ELEMENT_ID = 8;

    private const OFFICE_TRANSPORTATION_PLACE_ELEMENT_ID = 44;

    public static function withdrawalId(): self
    {
        return new self(self::WITHDRAWAL_PLACE_ELEMENT_ID);
    }

    public static function payCardId(): self
    {
        return new self(self::PAY_CARD_PLACE_ELEMENT_ID);
    }

    public static function skyId(): self
    {
        return new self(self::SKY_PLACE_ELEMENT_ID);
    }

    public static function salaryId(): self
    {
        return new self(self::SALARY_PLACE_ELEMENT_ID);
    }

    public static function officeTransportationId(): self
    {
        return new self(self::OFFICE_TRANSPORTATION_PLACE_ELEMENT_ID);
    }
}
