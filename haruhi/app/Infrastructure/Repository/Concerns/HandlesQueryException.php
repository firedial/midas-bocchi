<?php

namespace App\Infrastructure\Repository\Concerns;

use Illuminate\Database\QueryException;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;

trait HandlesQueryException
{
    private static function handleQueryException(QueryException $e): never
    {
        $code = $e->errorInfo[1] ?? null;

        match ($code) {
            1451 => throw new AppException(ErrorCode::DB_CHILD_RECORD_EXISTS),
            1452 => throw new AppException(ErrorCode::DB_PARENT_NOT_FOUND),
            default => throw $e,
        };
    }
}
