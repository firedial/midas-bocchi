<?php

namespace App\Infrastructure\Repository;

use App\Domain\Entities\FixedBalanceEntity;
use App\Domain\ValueObjects\FixedBalanceId;

interface FixedBalanceRepositoryInterface
{
    public function getFixedBalances(?FixedBalanceId $fixedBalanceId): array;
    public function selectFixedBalance(FixedBalanceId $fixedBalanceId): ?FixedBalanceEntity;
    public function insertFixedBalance(FixedBalanceEntity $fixedBalance): int;
    public function updateFixedBalance(FixedBalanceEntity $fixedBalance): void;
    public function deleteFixedBalance(FixedBalanceId $fixedBalanceId): void;
}
