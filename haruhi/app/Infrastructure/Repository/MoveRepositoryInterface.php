<?php

namespace App\Infrastructure\Repository;

use App\Domain\Entities\MoveEntity;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\MoveId;

interface MoveRepositoryInterface
{
    public function getMoves(Attribute $attribute, ?int $limit): array;
    public function selectMove(Attribute $attribute, MoveId $moveId): ?MoveEntity;
    public function insertMove(Attribute $attribute, MoveEntity $move): MoveEntity;
    public function updateMove(Attribute $attribute, MoveEntity $move): MoveEntity;
    public function deleteMove(Attribute $attribute, MoveId $moveId): MoveEntity;
}
