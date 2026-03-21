<?php

namespace App\Infrastructure\Repository\Impl;

use Illuminate\Database\QueryException;
use App\Infrastructure\Repository\Concerns\HandlesQueryException;
use App\Domain\Entities\FixedBalanceEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\FixedBalanceId;
use App\Domain\ValueObjects\Description;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Infrastructure\Repository\FixedBalanceRepositoryInterface;
use App\Models\DataModels\FixedBalanceDataModel;

class FixedBalanceRepositoryImpl implements FixedBalanceRepositoryInterface
{
    use HandlesQueryException;

    public function getFixedBalances(
        ?FixedBalanceId $fixedBalanceId = null,
    ): array {
        $fixedBalances = FixedBalanceDataModel::selectFixedBalance(id: $fixedBalanceId?->value());
        return array_map(
            function ($fixedBalance) {
                return new FixedBalanceEntity(
                    FixedBalanceId::filledId($fixedBalance->id),
                    new Amount($fixedBalance->amount),
                    new Item($fixedBalance->item),
                    KindElementId::filledId($fixedBalance->kind_element_id),
                    PurposeElementId::filledId($fixedBalance->purpose_element_id),
                    PlaceElementId::filledId($fixedBalance->place_element_id),
                    new Description($fixedBalance->kind_element_description),
                    new Description($fixedBalance->purpose_element_description),
                    new Description($fixedBalance->place_element_description),
                );
            },
            $fixedBalances
        );
    }

    public function selectFixedBalance(FixedBalanceId $fixedBalanceId): ?FixedBalanceEntity
    {
        $fixedBalances = $this->getFixedBalances(fixedBalanceId: $fixedBalanceId);
        if (count($fixedBalances) === 0) {
            return null;
        }
        return $fixedBalances[0];
    }

    public function insertFixedBalance(FixedBalanceEntity $fixedBalance): FixedBalanceEntity
    {
        try {
            $result = FixedBalanceDataModel::insertFixedBalance(
                $fixedBalance->amount()->value(),
                $fixedBalance->item()->value(),
                $fixedBalance->kindElementId()->value(),
                $fixedBalance->purposeElementId()->value(),
                $fixedBalance->placeElementId()->value(),
            );
        } catch (QueryException $e) {
            self::handleQueryException($e, 'Insert fixed balance error.');
        }

        return new FixedBalanceEntity(
            FixedBalanceId::filledId($result->id),
            new Amount($result->amount),
            new Item($result->item),
            KindElementId::filledId($result->kind_element_id),
            PurposeElementId::filledId($result->purpose_element_id),
            PlaceElementId::filledId($result->place_element_id),
        );
    }

    public function updateFixedBalance(FixedBalanceEntity $fixedBalance): FixedBalanceEntity
    {
        try {
            $result = FixedBalanceDataModel::updateFixedBalance(
                $fixedBalance->fixedBalanceId()->value(),
                $fixedBalance->amount()->value(),
                $fixedBalance->item()->value(),
                $fixedBalance->kindElementId()->value(),
                $fixedBalance->purposeElementId()->value(),
                $fixedBalance->placeElementId()->value(),
            );
        } catch (QueryException $e) {
            self::handleQueryException($e, 'Update fixed balance error.');
        }

        return new FixedBalanceEntity(
            FixedBalanceId::filledId($result->id),
            new Amount($result->amount),
            new Item($result->item),
            KindElementId::filledId($result->kind_element_id),
            PurposeElementId::filledId($result->purpose_element_id),
            PlaceElementId::filledId($result->place_element_id),
        );
    }

    public function deleteFixedBalance(FixedBalanceId $fixedBalanceId): FixedBalanceEntity
    {
        try {
            $result = FixedBalanceDataModel::deleteFixedBalance($fixedBalanceId->value());
        } catch (QueryException $e) {
            self::handleQueryException($e, 'Delete fixed balance error.');
        }

        return new FixedBalanceEntity(
            FixedBalanceId::filledId($result->id),
            new Amount($result->amount),
            new Item($result->item),
            KindElementId::filledId($result->kind_element_id),
            PurposeElementId::filledId($result->purpose_element_id),
            PlaceElementId::filledId($result->place_element_id),
        );
    }
}
