<?php

namespace App\Domain\Entities;

use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\AttributeElementId;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\Date;
use App\Domain\ValueObjects\Description;
use App\Domain\ValueObjects\MoveId;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;

class MoveEntity
{
    public function __construct(
        protected readonly MoveId $moveId,
        protected readonly Amount $amount,
        protected readonly Item $item,
        protected readonly AttributeElementId $beforeId,
        protected readonly AttributeElementId $afterId,
        protected readonly Date $date,
        protected readonly ?Description $beforeDescription = null,
        protected readonly ?Description $afterDescription = null,
    ) {}

    public function moveId(): MoveId
    {
        return $this->moveId;
    }

    public function amount(): Amount
    {
        return $this->amount;
    }

    public function item(): Item
    {
        return $this->item;
    }

    public function beforeId(): AttributeElementId
    {
        return $this->beforeId;
    }

    public function afterId(): AttributeElementId
    {
        return $this->afterId;
    }

    public function date(): Date
    {
        return $this->date;
    }

    public function beforeDescription(): Description
    {
        if (is_null($this->beforeDescription)) {
            throw new AppException(ErrorCode::INVALID_EMPTY, "Before description is null.");
        }
        return $this->beforeDescription;
    }

    public function afterDescription(): Description
    {
        if (is_null($this->afterDescription)) {
            throw new AppException(ErrorCode::INVALID_EMPTY, "After description is null.");
        }
        return $this->afterDescription;
    }
}
