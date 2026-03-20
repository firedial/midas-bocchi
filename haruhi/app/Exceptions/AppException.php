<?php

namespace App\Exceptions;

class AppException extends \RuntimeException
{
    public function __construct(
        public readonly ErrorCode $errorCode,
        string $detail,
    ) {
        parent::__construct($detail);
    }
}
