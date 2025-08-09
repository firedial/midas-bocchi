<?php

namespace App\Domain\Entities;

use App\Domain\ValueObjects\FixedBalanceId;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\Description;
use App\Exceptions\ValueObjectException;

class FixedBalanceEntity
{
    public function __construct(
        protected readonly FixedBalanceId $fixedBalanceId,
        protected readonly Amount $amount,
        protected readonly Item $item,
        protected readonly KindElementId $kindElementId,
        protected readonly PurposeElementId $purposeElementId,
        protected readonly PlaceElementId $placeElementId,
        protected readonly ?Description $kindElementDescription = null,
        protected readonly ?Description $purposeElementDescription = null,
        protected readonly ?Description $placeElementDescription = null,
    ) {}

    public function fixedBalanceId(): FixedBalanceId
    {
        return $this->fixedBalanceId;
    }

    public function amount(): Amount
    {
        return $this->amount;
    }

    public function item(): Item
    {
        return $this->item;
    }

    public function kindElementId(): KindElementId
    {
        return $this->kindElementId;
    }

    public function purposeElementId(): PurposeElementId
    {
        return $this->purposeElementId;
    }

    public function placeElementId(): placeElementId
    {
        return $this->placeElementId;
    }

    public function kindElementDescription(): Description
    {
        if (is_null($this->kindElementDescription)) {
            throw new ValueObjectException("Kind element description is null.");
        }
        return $this->kindElementDescription;
    }

    public function purposeElementDescription(): Description
    {
        if (is_null($this->purposeElementDescription)) {
            throw new ValueObjectException("Purpose element description is null.");
        }
        return $this->purposeElementDescription;
    }

    public function placeElementDescription(): Description
    {
        if (is_null($this->placeElementDescription)) {
            throw new ValueObjectException("Place element description is null.");
        }
        return $this->placeElementDescription;
    }
}
