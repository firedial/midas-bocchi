<?php

namespace App\Domain\ValueObjects;

class Attribute
{
    private const KIND = 1;
    private const PURPOSE = 2;
    private const PLACE = 3;

    private function __construct(private readonly int $attribute) {}

    public static function kind(): self
    {
        return new Attribute(self::KIND);
    }

    public static function purpose(): self
    {
        return new Attribute(self::PURPOSE);
    }

    public static function place(): self
    {
        return new Attribute(self::PLACE);
    }

    public function isKind(): bool
    {
        return $this->attribute === self::KIND;
    }

    public function isPurpose(): bool
    {
        return $this->attribute === self::PURPOSE;
    }

    public function isPlace(): bool
    {
        return $this->attribute === self::PLACE;
    }
}
