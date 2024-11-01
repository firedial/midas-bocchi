<?php

namespace App\Domain\ValueObject;

abstract class Id
{
    private string $id;

    public function __construct(string $id)
    {
        if (mb_strlen($id) === 0) {
            throw new Exception("wrong length");
        }

        if (preg_match('/^[\x20-\x7e]*$/', $id)) {
            // throw new Exception("wrong string");
        }

        $this->id = $id;
    }

    public function value(): string
    {
        return $this->id;
    }
}

