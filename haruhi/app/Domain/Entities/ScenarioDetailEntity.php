<?php

namespace App\Domain\Entities;

use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Item;

class ScenarioDetailEntity
{
    public function __construct(
        protected readonly int $seq,
        protected readonly int $type,
        protected readonly Amount $amount,
        protected readonly Item $item,
        protected readonly int $typeElementId,
        protected readonly ?int $purposeElementId,
        protected readonly ?int $placeElementId,
        protected readonly ?int $moveAttribute,
        protected readonly ?int $moveBeforeId,
        protected readonly ?int $moveAfterId,
    ) {}

    public function seq(): int
    {
        return $this->seq;
    }

    public function type(): int
    {
        return $this->type;
    }

    public function amount(): Amount
    {
        return $this->amount;
    }

    public function item(): Item
    {
        return $this->item;
    }

    public function typeElementId(): int
    {
        return $this->typeElementId;
    }

    public function purposeElementId(): ?int
    {
        return $this->purposeElementId;
    }

    public function placeElementId(): ?int
    {
        return $this->placeElementId;
    }

    public function moveAttribute(): ?int
    {
        return $this->moveAttribute;
    }

    public function moveBeforeId(): ?int
    {
        return $this->moveBeforeId;
    }

    public function moveAfterId(): ?int
    {
        return $this->moveAfterId;
    }
}
