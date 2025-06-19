<?php

namespace App\Infrastructure\Repository;

use App\Domain\Entities\BalanceEntity;
use App\Domain\ValueObjects\BalanceId;

interface BalanceRepositoryInterface
{
    // @todo ValueObject に変える
    // public function selectBalance(bool $isNotMoveOnly, bool $isMoveOnly, ?int $id, ?int $limit, ?string $orderBy): array;
    public function selectBalance(BalanceId $balanceId): ?BalanceEntity;
    public function insertBalance(BalanceEntity $balance): int;
    public function updateBalance(BalanceEntity $balance): void;
    public function deleteBalance(BalanceId $balanceId): void;
}
