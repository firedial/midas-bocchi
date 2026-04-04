<?php

namespace App\Domain\Entities;

use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Item;

class TemplateDetailEntity
{
    public function __construct(
        protected readonly int $seq,
        protected readonly int $type,
        protected readonly Amount $amount,
        protected readonly Item $item,
        protected readonly int $kindElementId,
        protected readonly ?int $purposeElementId,
        protected readonly ?int $placeElementId,
        protected readonly ?int $moveBeforePurposeId,
        protected readonly ?int $moveAfterPurposeId,
        protected readonly ?int $moveBeforePlaceId,
        protected readonly ?int $moveAfterPlaceId,
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

    public function kindElementId(): int
    {
        return $this->kindElementId;
    }

    public function purposeElementId(): ?int
    {
        return $this->purposeElementId;
    }

    public function placeElementId(): ?int
    {
        return $this->placeElementId;
    }

    public function moveBeforePurposeId(): ?int
    {
        return $this->moveBeforePurposeId;
    }

    public function moveAfterPurposeId(): ?int
    {
        return $this->moveAfterPurposeId;
    }

    public function moveBeforePlaceId(): ?int
    {
        return $this->moveBeforePlaceId;
    }

    public function moveAfterPlaceId(): ?int
    {
        return $this->moveAfterPlaceId;
    }
}
