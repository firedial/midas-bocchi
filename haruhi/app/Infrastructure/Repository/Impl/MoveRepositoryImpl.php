<?php

namespace App\Infrastructure\Repository\Impl;

use App\Domain\Entities\BalanceEntity;
use App\Domain\Entities\MoveEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\AttributeElementId;
use App\Domain\ValueObjects\BalanceId;
use App\Domain\ValueObjects\Date;
use App\Domain\ValueObjects\Description;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\MoveId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Exceptions\InternalException;
use App\Infrastructure\Repository\MoveRepositoryInterface;
use App\Models\DataModels\BalanceDataModel;

class MoveRepositoryImpl implements MoveRepositoryInterface
{
    public function getMoves(
        Attribute $attribute,
        ?int $limit = null,
    ): array {
        $purposeElementId = match (true) {
            $attribute->isPurpose() => null,
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        $placeElementId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => null,
            default => throw new InternalException('Attribute name is wrong.'),
        };

        $balances = BalanceDataModel::selectBalance(
            purposeElementId: $purposeElementId?->value(),
            placeElementId: $placeElementId?->value(),
            limit: is_null($limit) ? null : $limit * 2,
            orderByDesc: true,
        );

        $moves = [];
        foreach (array_chunk($balances, 2) as $balanceMove) {
            $after = $balanceMove[0];
            $before = $balanceMove[1];

            $beforeElementId = match (true) {
                $attribute->isPurpose() => AttributeElementId::filledId($before->purpose_element_id),
                $attribute->isPlace() => AttributeElementId::filledId($before->place_element_id),
                default => throw new InternalException('Attribute name is wrong.'),
            };

            $afterElementId = match (true) {
                $attribute->isPurpose() => AttributeElementId::filledId($after->purpose_element_id),
                $attribute->isPlace() => AttributeElementId::filledId($after->place_element_id),
                default => throw new InternalException('Attribute name is wrong.'),
            };

            $beforeElementDescription = match (true) {
                $attribute->isPurpose() => new Description($before->purpose_element_description),
                $attribute->isPlace() => new Description($before->place_element_description),
                default => throw new InternalException('Attribute name is wrong.'),
            };

            $afterElementDescription = match (true) {
                $attribute->isPurpose() => new Description($after->purpose_element_description),
                $attribute->isPlace() => new Description($after->place_element_description),
                default => throw new InternalException('Attribute name is wrong.'),
            };

            $moves[] = new MoveEntity(
                MoveId::filledId($before->id),
                new Amount($after->amount),
                new Item($before->item),
                $beforeElementId,
                $afterElementId,
                new Date($before->date),
                beforeDescription: $beforeElementDescription,
                afterDescription: $afterElementDescription,
            );
        }

        return $moves;
    }

    public function selectMove(Attribute $attribute, MoveId $moveId): ?MoveEntity
    {
        $purposeElementId = match (true) {
            $attribute->isPurpose() => null,
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        $placeElementId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => null,
            default => throw new InternalException('Attribute name is wrong.'),
        };

        $befores = BalanceDataModel::selectBalance(
            purposeElementId: $purposeElementId?->value(),
            placeElementId: $placeElementId?->value(),
            id: $moveId->value(),
        );
        $afters = BalanceDataModel::selectBalance(
            purposeElementId: $purposeElementId?->value(),
            placeElementId: $placeElementId?->value(),
            id: $moveId->value() + 1,
        );

        if (count($befores) === 0 || count($afters) === 0) {
            return null;
        }

        $before = $befores[0];
        $after = $afters[0];

        $beforeElementId = match (true) {
            $attribute->isPurpose() => AttributeElementId::filledId($before->purpose_element_id),
            $attribute->isPlace() => AttributeElementId::filledId($before->place_element_id),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        $afterElementId = match (true) {
            $attribute->isPurpose() => AttributeElementId::filledId($after->purpose_element_id),
            $attribute->isPlace() => AttributeElementId::filledId($after->place_element_id),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        $beforeElementDescription = match (true) {
            $attribute->isPurpose() => new Description($before->purpose_element_description),
            $attribute->isPlace() => new Description($before->place_element_description),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        $afterElementDescription = match (true) {
            $attribute->isPurpose() => new Description($after->purpose_element_description),
            $attribute->isPlace() => new Description($after->place_element_description),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        return new MoveEntity(
            MoveId::filledId($before->id),
            new Amount($after->amount),
            new Item($before->item),
            $beforeElementId,
            $afterElementId,
            new Date($before->date),
            beforeDescription: $beforeElementDescription,
            afterDescription: $afterElementDescription,
        );
    }

    public function insertMove(Attribute $attribute, MoveEntity $move): int
    {
        if ($move->amount()->value() < 0) {
            throw new InternalException('Move amount needs positive amount.');
        }

        $beforePurposeElementId = match (true) {
            $attribute->isPurpose() => $move->beforeId(),
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        $afterPurposeElementId = match (true) {
            $attribute->isPurpose() => $move->afterId(),
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        $beforePlaceElementId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => $move->beforeId(),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        $afterPlaceElementId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => $move->afterId(),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        $beforeId = BalanceDataModel::insertBalance(
            (-1) * $move->amount()->value(),
            $move->item()->value(),
            KindElementId::moveId()->value(),
            $beforePurposeElementId->value(),
            $beforePlaceElementId->value(),
            $move->date()->value(),
        );

        $afterId = BalanceDataModel::insertBalance(
            $move->amount()->value(),
            $move->item()->value(),
            KindElementId::moveId()->value(),
            $afterPurposeElementId->value(),
            $afterPlaceElementId->value(),
            $move->date()->value(),
        );

        // 1違いでなければ例外
        if ($afterId !== $beforeId + 1) {
            throw new InternalException('After id is wrong.');
        }

        return $beforeId;
    }

    public function updateMove(Attribute $attribute, MoveEntity $move): void
    {
        if ($move->amount()->value() < 0) {
            throw new InternalException('Move amount needs positive amount.');
        }

        $beforePurposeElementId = match (true) {
            $attribute->isPurpose() => $move->beforeId(),
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        $afterPurposeElementId = match (true) {
            $attribute->isPurpose() => $move->afterId(),
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        $beforePlaceElementId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => $move->beforeId(),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        $afterPlaceElementId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => $move->afterId(),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        BalanceDataModel::updateBalance(
            $move->moveId()->value(),
            (-1) * $move->amount()->value(),
            $move->item()->value(),
            KindElementId::moveId()->value(),
            $beforePurposeElementId->value(),
            $beforePlaceElementId->value(),
            $move->date()->value(),
        );

        BalanceDataModel::updateBalance(
            $move->moveId()->value() + 1,
            $move->amount()->value(),
            $move->item()->value(),
            KindElementId::moveId()->value(),
            $afterPurposeElementId->value(),
            $afterPlaceElementId->value(),
            $move->date()->value(),
        );
    }

    public function deleteMove(MoveId $moveId): void
    {
        BalanceDataModel::deleteBalance($moveId->value());
        BalanceDataModel::deleteBalance($moveId->value() + 1);
    }
}
