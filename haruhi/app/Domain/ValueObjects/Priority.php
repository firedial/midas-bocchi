<?php

namespace App\Domain\ValueObjects;

use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;

class Priority
{
    public function __construct(private readonly int $priority)
    {
        if ($priority < 0 || $priority > 100) {
            throw new AppException(ErrorCode::INVALID_RANGE, "Priority is wrong.");
        }
    }

    public function value(): int
    {
        return $this->priority;
    }
}
