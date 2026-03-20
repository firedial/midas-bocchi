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

    case DB_PARENT_NOT_FOUND = 'E301';
    case DB_CHILD_RECORD_EXISTS = 'E302';

    case OTHER_ERROR = 'E999';

    public function message(): string
    {
        return match ($this) {
            self::DB_PARENT_NOT_FOUND => '参照先のレコードが存在しません',
            self::DB_CHILD_RECORD_EXISTS => '他のレコードから参照されています',
            self::OTHER_ERROR => '予期せぬエラーです',
        };
    }

    public function httpStatus(): int
    {
        return match ($this) {
            self::DB_PARENT_NOT_FOUND => 400,
            self::DB_CHILD_RECORD_EXISTS => 400,
            self::OTHER_ERROR => 500,
        };
    }
}
