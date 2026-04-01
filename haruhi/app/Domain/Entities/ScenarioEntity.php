<?php

namespace App\Domain\Entities;

use App\Domain\ValueObjects\ScenarioId;

class ScenarioEntity
{
    public function __construct(
        protected readonly ScenarioId $scenarioId,
        protected readonly string $name,
        protected readonly array $details = [],
    ) {}

    public function scenarioId(): ScenarioId
    {
        return $this->scenarioId;
    }

    public function name(): string
    {
        return $this->name;
    }

    /** @return ScenarioDetailEntity[] */
    public function details(): array
    {
        return $this->details;
    }
}
