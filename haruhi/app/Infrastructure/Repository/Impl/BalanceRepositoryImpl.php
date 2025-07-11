<?php

namespace App\Infrastructure\Repository\Impl;

use App\Domain\Entities\BalanceEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\BalanceId;
use App\Domain\ValueObjects\Date;
use App\Domain\ValueObjects\Description;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Infrastructure\Repository\BalanceRepositoryInterface;
use App\Models\DataModels\BalanceDataModel;

class BalanceRepositoryImpl implements BalanceRepositoryInterface
{
    public function getBalances(
        ?BalanceId $balanceId = null,
        ?int $limit = null,
        ?bool $orderByDesc = null,
    ): array {
        $balances = BalanceDataModel::selectBalance(notKindElementId: KindElementId::moveId()->value(), id: $balanceId?->value(), limit: $limit, orderByDesc: $orderByDesc);
        return array_map(
            function ($balance) {
                return new BalanceEntity(
                    BalanceId::filledId($balance->id),
                    new Amount($balance->amount),
                    new Item($balance->item),
                    KindElementId::filledId($balance->kind_element_id),
                    PurposeElementId::filledId($balance->purpose_element_id),
                    PlaceElementId::filledId($balance->place_element_id),
                    new Date($balance->date),
                    new Description($balance->kind_element_description),
                    new Description($balance->purpose_element_description),
                    new Description($balance->place_element_description),
                );
            },
            $balances
        );
    }

    public function selectBalance(BalanceId $balanceId): ?BalanceEntity
    {
        $balances = $this->getBalances(balanceId: $balanceId);
        if (count($balances) === 0) {
            return null;
        }
        return $balances[0];
    }

    public function insertBalance(BalanceEntity $balance): int
    {
        return BalanceDataModel::insertBalance(
            $balance->amount()->value(),
            $balance->item()->value(),
            $balance->kindElementId()->value(),
            $balance->purposeElementId()->value(),
            $balance->placeElementId()->value(),
            $balance->date()->value(),
        );
    }

    public function updateBalance(BalanceEntity $balance): void
    {
        BalanceDataModel::updateBalance(
            $balance->balanceId()->value(),
            $balance->amount()->value(),
            $balance->item()->value(),
            $balance->kindElementId()->value(),
            $balance->purposeElementId()->value(),
            $balance->placeElementId()->value(),
            $balance->date()->value(),
        );
    }

    public function deleteBalance(BalanceId $balanceId): void
    {
        BalanceDataModel::deleteBalance($balanceId->value());
    }

    public function sum(PlaceElementId $placeElementId): int
    {
        return BalanceDataModel::sum($placeElementId->value());
    }
}
