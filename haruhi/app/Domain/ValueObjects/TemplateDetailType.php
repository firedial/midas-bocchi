<?php

namespace App\Domain\ValueObjects;

use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;

class TemplateDetailType
{
    private const BALANCE = 1;
    private const PURPOSE_MOVE = 2;
    private const PLACE_MOVE = 3;

    private const VALID_TYPES = [self::BALANCE, self::PURPOSE_MOVE, self::PLACE_MOVE];

    public function __construct(private readonly int $type)
    {
        if (!in_array($type, self::VALID_TYPES, true)) {
            throw new AppException(ErrorCode::INVALID_VALUE, "TemplateDetailType is invalid.");
        }
    }

    public function value(): int
    {
        return $this->type;
    }

    public function isBalance(): bool
    {
        return $this->type === self::BALANCE;
    }

    public function isPurposeMove(): bool
    {
        return $this->type === self::PURPOSE_MOVE;
    }

    public function isPlaceMove(): bool
    {
        return $this->type === self::PLACE_MOVE;
    }
}
