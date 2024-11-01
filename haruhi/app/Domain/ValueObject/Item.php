<?php

namespace App\Domain\ValueObject;

class Item
{
    const MAX_LENGTH = 50;
    private string $string;

    public function __construct(string $string)
    {
        if (mb_strlen($string) > self::MAX_LENGTH) {
            throw new Exception("wrong length");
        }

        if (mb_strlen($string) === 0) {
            throw new Exception("wrong length");
        }

        $this->string = $string;
    }

    public function value(): string
    {
        return $this->string;
    }
}
