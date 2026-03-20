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

    case RECORD_NOT_FOUND = 'E301';
    case PARENT_RECORD_NOT_FOUND = 'E302';
    case CHILD_RECORD_EXISTS = 'E303';
    case DUPLICATE_ENTRY = 'E304';

    case UNEXPECTED_ATTRIBUTE_NAME = 'E901';
    case OTHER_ERROR = 'E999';

    public function httpStatus(): int
    {
        return match ($this) {
            self::UNEXPECTED_ATTRIBUTE_NAME => 500,
            self::OTHER_ERROR => 500,
            default => 400,
        };
    }
}
