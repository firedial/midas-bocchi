<?php

namespace App\Domain\ValueObjects;

use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;

class AttributeElementName
{
    private const MAX_LENGTH = 20;

    public function __construct(private readonly string $name)
    {
        if (mb_strlen($name) === 0) {
            throw new AppException(ErrorCode::INVALID_LENGTH, "Name length is empty.");
        }
        if (mb_strlen($name) > self::MAX_LENGTH) {
            throw new AppException(ErrorCode::INVALID_LENGTH, "Name length is over.");
        }
        if (!preg_match('/^[a-z]\w*$/', $name)) {
            throw new AppException(ErrorCode::INVALID_FORMAT, "Name contains invalid characters.");
        }
    }

    public function value(): string
    {
        return $this->name;
    }
}
