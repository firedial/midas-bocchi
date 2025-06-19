<?php

namespace App\Domain\Entities;

use App\Domain\ValueObjects\BalanceId;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\Date;

class BalanceEntity
{
    public function __construct(
        protected readonly BalanceId $balanceId,
        protected readonly Amount $amount,
        protected readonly Item $item,
        protected readonly KindElementId $kindElementId,
        protected readonly PurposeElementId $purposeElementId,
        protected readonly PlaceElementId $placeElementId,
        protected readonly Date $date,
    ) {}

    public function balanceId(): BalanceId
    {
        return $this->balanceId;
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

    public function date(): Date
    {
        return $this->date;
    }

    public function isMove(): bool
    {
        return $this->kindElementId()->isMoveId();
    }
}
