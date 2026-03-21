<?php

namespace App\Domain\ValueObjects;

use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;

class Description
{
    private const MAX_LENGTH = 20;

    public function __construct(private readonly string $description)
    {
        if (mb_strlen($description) === 0) {
            throw new AppException(ErrorCode::INVALID_EMPTY, "Description length is empty.");
        }
        if (mb_strlen($description) > self::MAX_LENGTH) {
            throw new AppException(ErrorCode::INVALID_LENGTH, "Description length is over.");
        }
    }

    public function value(): string
    {
        return $this->description;
    }
}
