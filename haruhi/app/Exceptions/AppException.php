<?php

namespace App\Exceptions;

class AppException extends \RuntimeException
{
    public function __construct(
        public readonly ErrorCode $errorCode,
        ?string $detail = null,
    ) {
        parent::__construct($detail ?? $errorCode->value);
    }
}
