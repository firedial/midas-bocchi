<?php

namespace App\Domain\Entities;

use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Domain\ValueObjects\TemplateDetailType;

class TemplateDetailEntity
{
    public function __construct(
        protected readonly int $seq,
        protected readonly TemplateDetailType $type,
        protected readonly Amount $amount,
        protected readonly Item $item,
        protected readonly ?KindElementId $kindElementId,
        protected readonly ?PurposeElementId $purposeElementId,
        protected readonly ?PlaceElementId $placeElementId,
        protected readonly ?PurposeElementId $moveBeforePurposeId,
        protected readonly ?PurposeElementId $moveAfterPurposeId,
        protected readonly ?PlaceElementId $moveBeforePlaceId,
        protected readonly ?PlaceElementId $moveAfterPlaceId,
    ) {}

    public function seq(): int
    {
        return $this->seq;
    }

    public function type(): TemplateDetailType
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

    public function kindElementId(): ?KindElementId
    {
        return $this->kindElementId;
    }

    public function purposeElementId(): ?PurposeElementId
    {
        return $this->purposeElementId;
    }

    public function placeElementId(): ?PlaceElementId
    {
        return $this->placeElementId;
    }

    public function moveBeforePurposeId(): ?PurposeElementId
    {
        return $this->moveBeforePurposeId;
    }

    public function moveAfterPurposeId(): ?PurposeElementId
    {
        return $this->moveAfterPurposeId;
    }

    public function moveBeforePlaceId(): ?PlaceElementId
    {
        return $this->moveBeforePlaceId;
    }

    public function moveAfterPlaceId(): ?PlaceElementId
    {
        return $this->moveAfterPlaceId;
    }
}
