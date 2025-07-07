<?php

namespace App\Infrastructure\Repository\Impl;

use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\Date;
use App\Infrastructure\Repository\CheckPlaceSumRepositoryInterface;
use App\Models\DataModels\CheckPlaceSumDataModel;

class CheckPlaceSumRepositoryImpl implements CheckPlaceSumRepositoryInterface
{
    public function insertCheckPlaceSum(int $sum, PlaceElementId $placeElementId, Date $date): int
    {
        return CheckPlaceSumDataModel::insert($sum, $placeElementId->value(), $date->value());
    }
}
