<?php

namespace App\Domain\ValueObjects;

class KindElementId extends AttributeElementId
{
    protected const MOVE_ID = 1;

    private const HOUSE_RENT_KIND_ELEMENT_ID = 8;
    private const GAS_KIND_ELEMENT_ID = 9;
    private const WATER_KIND_ELEMENT_ID = 10;
    private const ELECT_KIND_ELEMENT_ID = 11;
    private const NET_KIND_ELEMENT_ID = 12;

    private const SALARY_KIND_ELEMENT_ID = 14;
    private const TRANSPORTATION_KIND_ELEMENT_ID = 17;
    private const HOLDING_KIND_ELEMENT_ID = 18;
    private const DEDUCTION_KIND_ELEMENT_ID = 16;
    private const SHARE_HELD_KIND_ELEMENT_ID = 19;

    private const OFFICE_TRANSPORTATION_KIND_ELEMENT_ID = 20;

    public static function houseId(): self
    {
        return new self(self::HOUSE_RENT_KIND_ELEMENT_ID);
    }

    public static function gasId(): self
    {
        return new self(self::GAS_KIND_ELEMENT_ID);
    }

    public static function waterId(): self
    {
        return new self(self::WATER_KIND_ELEMENT_ID);
    }

    public static function electId(): self
    {
        return new self(self::ELECT_KIND_ELEMENT_ID);
    }

    public static function netId(): self
    {
        return new self(self::NET_KIND_ELEMENT_ID);
    }

    public static function salaryId(): self
    {
        return new self(self::SALARY_KIND_ELEMENT_ID);
    }

    public static function transportationId(): self
    {
        return new self(self::TRANSPORTATION_KIND_ELEMENT_ID);
    }

    public static function holdingId(): self
    {
        return new self(self::HOLDING_KIND_ELEMENT_ID);
    }

    public static function deductionId(): self
    {
        return new self(self::DEDUCTION_KIND_ELEMENT_ID);
    }

    public static function shareHeldId(): self
    {
        return new self(self::SHARE_HELD_KIND_ELEMENT_ID);
    }

    public static function officeTransportationId(): self
    {
        return new self(self::OFFICE_TRANSPORTATION_KIND_ELEMENT_ID);
    }
}
