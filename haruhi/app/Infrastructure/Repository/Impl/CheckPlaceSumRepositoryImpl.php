<?php

namespace App\Infrastructure\Repository\Impl;

use Illuminate\Database\QueryException;
use App\Infrastructure\Repository\Concerns\HandlesQueryException;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\Date;
use App\Infrastructure\Repository\CheckPlaceSumRepositoryInterface;
use App\Models\DataModels\CheckPlaceSumDataModel;

class CheckPlaceSumRepositoryImpl implements CheckPlaceSumRepositoryInterface
{
    use HandlesQueryException;

    public function insertCheckPlaceSum(int $sum, PlaceElementId $placeElementId, Date $date): int
    {
        try {
            return CheckPlaceSumDataModel::insert($sum, $placeElementId->value(), $date->value());
        } catch (QueryException $e) {
            self::handleQueryException($e);
        }
    }
}
