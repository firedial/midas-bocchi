<?php

namespace App\Exceptions;

enum ErrorCode: string
{
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
