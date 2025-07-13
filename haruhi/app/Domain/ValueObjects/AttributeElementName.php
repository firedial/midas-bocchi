<?php

namespace App\Domain\ValueObjects;

use App\Exceptions\ValueObjectException;

class AttributeElementName
{
    private const MAX_LENGTH = 20;

    public function __construct(private readonly string $name)
    {
        if (mb_strlen($name) === 0 || mb_strlen($name) > self::MAX_LENGTH) {
            throw new ValueObjectException("Name length is over.");
        }
        if (!preg_match('/^[a-z]\w*$/', $name)) {
            throw new ValueObjectException("Name contains invalid characters.");
        }
    }

    public function value(): string
    {
        return $this->name;
    }
}
