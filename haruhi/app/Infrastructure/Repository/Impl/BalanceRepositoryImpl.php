<?php

namespace App\Infrastructure\Repository\Impl;

use Illuminate\Database\QueryException;
use App\Infrastructure\Repository\Concerns\HandlesQueryException;
use App\Domain\Entities\BalanceEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\BalanceId;
use App\Domain\ValueObjects\Date;
use App\Domain\ValueObjects\Description;
use App\Domain\ValueObjects\GroupId;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Infrastructure\Repository\BalanceRepositoryInterface;
use App\Models\DataModels\BalanceDataModel;
use stdClass;

class BalanceRepositoryImpl implements BalanceRepositoryInterface
{
    use HandlesQueryException;

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
                    is_null($balance->group_id) ? null : GroupId::filledId($balance->group_id),
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

    public function insertBalance(BalanceEntity $balance): BalanceEntity
    {
        try {
            $result = BalanceDataModel::insertBalance(
                $balance->amount()->value(),
                $balance->item()->value(),
                $balance->kindElementId()->value(),
                $balance->purposeElementId()->value(),
                $balance->placeElementId()->value(),
                $balance->date()->value(),
                $balance->groupId()?->value(),
            );
        } catch (QueryException $e) {
            self::handleQueryException($e, 'Insert balance error.');
        }

        return new BalanceEntity(
            BalanceId::filledId($result->id),
            new Amount($result->amount),
            new Item($result->item),
            KindElementId::filledId($result->kind_element_id),
            PurposeElementId::filledId($result->purpose_element_id),
            PlaceElementId::filledId($result->place_element_id),
            new Date($result->date),
            is_null($result->group_id) ? null : GroupId::filledId($result->group_id),
        );
    }

    public function updateBalance(BalanceEntity $balance): BalanceEntity
    {
        try {
            $result = BalanceDataModel::updateBalance(
                $balance->balanceId()->value(),
                $balance->amount()->value(),
                $balance->item()->value(),
                $balance->kindElementId()->value(),
                $balance->purposeElementId()->value(),
                $balance->placeElementId()->value(),
                $balance->date()->value(),
                $balance->groupId()?->value(),
            );
        } catch (QueryException $e) {
            self::handleQueryException($e, 'Update balance error.');
        }

        return new BalanceEntity(
            BalanceId::filledId($result->id),
            new Amount($result->amount),
            new Item($result->item),
            KindElementId::filledId($result->kind_element_id),
            PurposeElementId::filledId($result->purpose_element_id),
            PlaceElementId::filledId($result->place_element_id),
            new Date($result->date),
            is_null($result->group_id) ? null : GroupId::filledId($result->group_id),
        );
    }

    public function deleteBalance(BalanceId $balanceId): BalanceEntity
    {
        try {
            $result = BalanceDataModel::deleteBalance($balanceId->value());
        } catch (QueryException $e) {
            self::handleQueryException($e, 'Delete balance error.');
        }

        return new BalanceEntity(
            BalanceId::filledId($result->id),
            new Amount($result->amount),
            new Item($result->item),
            KindElementId::filledId($result->kind_element_id),
            PurposeElementId::filledId($result->purpose_element_id),
            PlaceElementId::filledId($result->place_element_id),
            new Date($result->date),
            is_null($result->group_id) ? null : GroupId::filledId($result->group_id),
        );
    }

    public function sum(PlaceElementId $placeElementId): int
    {
        return BalanceDataModel::sum($placeElementId->value());
    }
}
