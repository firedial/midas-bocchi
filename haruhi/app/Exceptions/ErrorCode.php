<?php

namespace App\Exceptions;

enum ErrorCode: string
{
    case INVALID_TYPE = 'E101';
    case INVALID_RANGE = 'E102';
    case INVALID_FORMAT = 'E103';
    case INVALID_EMPTY = 'E104';
    case INVALID_LENGTH = 'E105';
    case INVALID_VALUE = 'E106';
    case MOVE_SAME_ID = 'E107';
    case USING_MOVE_ID = 'E108';
    case MISSING_REQUIRED = 'E109';

    case UNAUTHORIZED = 'E201';

    case RECORD_NOT_FOUND = 'E301';
    case PARENT_RECORD_NOT_FOUND = 'E302';
    case CHILD_RECORD_EXISTS = 'E303';
    case DUPLICATE_ENTRY = 'E304';
    case PAGE_NOT_FOUND = 'E305';

    case UNEXPECTED_ATTRIBUTE_NAME = 'E901';
    case UNEXPECTED_AMOUNT = 'E902';
    case UNEXPECTED_DIFFERENCE_ID_MOVE = 'E903';
    case UNEXPECTED = 'E999';

    public function httpStatus(): int
    {
        return match ($this) {
            self::UNAUTHORIZED => 401,
            self::PAGE_NOT_FOUND => 404,
            self::RECORD_NOT_FOUND => 404,
            self::UNEXPECTED_ATTRIBUTE_NAME => 500,
            self::UNEXPECTED_AMOUNT => 500,
            self::UNEXPECTED_DIFFERENCE_ID_MOVE => 500,
            self::UNEXPECTED => 500,
            default => 400,
        };
    }
}
