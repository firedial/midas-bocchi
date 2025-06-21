<?php

namespace App\Domain\ValueObjects;

class PurposeElementId extends AttributeElementId
{
    protected const MOVE_ID = 1;

    private const UTIL_COST_PURPOSE_ELEMENT_ID = 10;

    private const INCOME_PURPOSE_ELEMENT_ID = 3;
    private const TRANSPORTATION_PURPOSE_ELEMENT_ID = 4;
    private const DEDUCTION_PURPOSE_ELEMENT_ID = 14;

    public static function utilCostId(): self
    {
        return new self(self::UTIL_COST_PURPOSE_ELEMENT_ID);
    }

    public static function incomeId(): self
    {
        return new self(self::INCOME_PURPOSE_ELEMENT_ID);
    }

    public static function transportationId(): self
    {
        return new self(self::TRANSPORTATION_PURPOSE_ELEMENT_ID);
    }

    public static function deductionId(): self
    {
        return new self(self::DEDUCTION_PURPOSE_ELEMENT_ID);
    }
}
