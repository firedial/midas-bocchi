<?php

namespace App\Infrastructure\Repository;

use App\Domain\Entities\BalanceEntity;
use App\Domain\ValueObjects\BalanceId;
use App\Domain\ValueObjects\PlaceElementId;

interface BalanceRepositoryInterface
{
    public function getBalances(?BalanceId $balanceId, ?int $limit, ?bool $orderByDesc): array;
    public function selectBalance(BalanceId $balanceId): ?BalanceEntity;
    public function insertBalance(BalanceEntity $balance): BalanceEntity;
    public function updateBalance(BalanceEntity $balance): BalanceEntity;
    public function deleteBalance(BalanceId $balanceId): BalanceEntity;

    public function sum(PlaceElementId $placeElementId): int;
}
