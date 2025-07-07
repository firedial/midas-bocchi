<?php

namespace App\Infrastructure\Repository;

use App\Domain\ValueObjects\Date;
use App\Domain\ValueObjects\PlaceElementId;

interface CheckPlaceSumRepositoryInterface
{
    public function insertCheckPlaceSum(int $sum, PlaceElementId $placeKindId, Date $date): int;
}
