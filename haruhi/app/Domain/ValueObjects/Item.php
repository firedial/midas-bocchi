<?php

namespace App\Domain\ValueObjects;

use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;

class Item
{
    private const MAX_LENGTH = 50;

    public function __construct(private readonly string $item)
    {
        if (mb_strlen($item) === 0) {
            throw new AppException(ErrorCode::INVALID_LENGTH, "Item length is empty.");
        }
        if (mb_strlen($item) > self::MAX_LENGTH) {
            throw new AppException(ErrorCode::INVALID_LENGTH, "Item length is over.");
        }
    }

    public function value(): string
    {
        return $this->item;
    }
}
