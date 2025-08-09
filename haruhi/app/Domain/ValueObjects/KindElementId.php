<?php

namespace App\Domain\ValueObjects;

class KindElementId extends AttributeElementId
{
    protected const MOVE_ID = 1;

    private const SALARY_KIND_ELEMENT_ID = 14;
    private const TRANSPORTATION_KIND_ELEMENT_ID = 17;
    private const HOLDING_KIND_ELEMENT_ID = 18;
    private const DEDUCTION_KIND_ELEMENT_ID = 16;
    private const SHARE_HELD_KIND_ELEMENT_ID = 19;

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
}
