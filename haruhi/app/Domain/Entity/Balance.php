<?php

namespace App\Domain\Entity;

use App\Domain\ValueObject\BalanceId;
use App\Domain\ValueObject\Amount;
use App\Domain\ValueObject\Item;
use App\Domain\ValueObject\KindId;
use App\Domain\ValueObject\PurposeId;
use App\Domain\ValueObject\PlaceId;
use App\Domain\ValueObject\Date;

class Balance
{

    public function __construct(
        private readonly Amount $amount,
        private readonly Item $item,
        private readonly KindId $kindId,
        private readonly PurposeId $purposeId,
        private readonly PlaceId $placeId,
        private readonly Date $date,
    ) {
    }

    public function getAmount(): Amount
    {
        return $this->amount->value();
    }

    public function getItem(): Item
    {
        return $this->item->value();
    }

    public function getKindId(): KindId
    {
        return $this->kindId->value();
    }

    public function getPurposeId(): PurposeId
    {
        return $this->purposeId->value();
    }

    public function getPlaceId(): PlaceId
    {
        return $this->placeId->value();
    }

    public function getDate(): Date
    {
        return $this->date->value();
    }
}

