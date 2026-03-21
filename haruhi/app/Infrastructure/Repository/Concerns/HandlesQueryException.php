<?php

namespace App\Infrastructure\Repository\Concerns;

use Illuminate\Database\QueryException;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;

trait HandlesQueryException
{
    private static function handleQueryException(QueryException $e, string $message): never
    {
        $code = $e->errorInfo[1] ?? null;

        match ($code) {
            1062 => throw new AppException(ErrorCode::DUPLICATE_ENTRY, 'Duplicate entry. ' . $message),
            1451 => throw new AppException(ErrorCode::CHILD_RECORD_EXISTS, 'Child record exists. ' . $message),
            1452 => throw new AppException(ErrorCode::PARENT_RECORD_NOT_FOUND, 'Parent record not found. ' . $message),
            default => throw $e,
        };
    }
}
