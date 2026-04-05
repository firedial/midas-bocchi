<?php

namespace App\Infrastructure\Repository\Impl;

use Illuminate\Database\QueryException;
use App\Infrastructure\Repository\Concerns\HandlesQueryException;
use App\Domain\Entities\MoveEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\AttributeElementId;
use App\Domain\ValueObjects\Date;
use App\Domain\ValueObjects\Description;
use App\Domain\ValueObjects\GroupId;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\MoveId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Infrastructure\Repository\MoveRepositoryInterface;
use App\Models\DataModels\BalanceDataModel;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;

class MoveRepositoryImpl implements MoveRepositoryInterface
{
    use HandlesQueryException;

    public function getMoves(
        Attribute $attribute,
        ?int $limit = null,
    ): array {
        $purposeElementId = match (true) {
            $attribute->isPurpose() => null,
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $placeElementId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => null,
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
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
                default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
            };

            $afterElementId = match (true) {
                $attribute->isPurpose() => AttributeElementId::filledId($after->purpose_element_id),
                $attribute->isPlace() => AttributeElementId::filledId($after->place_element_id),
                default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
            };

            $beforeElementDescription = match (true) {
                $attribute->isPurpose() => new Description($before->purpose_element_description),
                $attribute->isPlace() => new Description($before->place_element_description),
                default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
            };

            $afterElementDescription = match (true) {
                $attribute->isPurpose() => new Description($after->purpose_element_description),
                $attribute->isPlace() => new Description($after->place_element_description),
                default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
            };

            if (is_null($before->group_id)) {
                throw new AppException(ErrorCode::UNEXPECTED_NULL_READ, 'Group id is null.');
            }

            $moves[] = new MoveEntity(
                MoveId::filledId($before->id),
                new Amount($after->amount),
                new Item($before->item),
                $beforeElementId,
                $afterElementId,
                new Date($before->date),
                GroupId::filledId($before->group_id),
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
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $placeElementId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => null,
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
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
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $afterElementId = match (true) {
            $attribute->isPurpose() => AttributeElementId::filledId($after->purpose_element_id),
            $attribute->isPlace() => AttributeElementId::filledId($after->place_element_id),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $beforeElementDescription = match (true) {
            $attribute->isPurpose() => new Description($before->purpose_element_description),
            $attribute->isPlace() => new Description($before->place_element_description),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $afterElementDescription = match (true) {
            $attribute->isPurpose() => new Description($after->purpose_element_description),
            $attribute->isPlace() => new Description($after->place_element_description),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        if (is_null($before->group_id)) {
            throw new AppException(ErrorCode::UNEXPECTED_NULL_READ, 'Group id is null.');
        }

        return new MoveEntity(
            MoveId::filledId($before->id),
            new Amount($after->amount),
            new Item($before->item),
            $beforeElementId,
            $afterElementId,
            new Date($before->date),
            GroupId::filledId($before->group_id),
            beforeDescription: $beforeElementDescription,
            afterDescription: $afterElementDescription,
        );
    }

    public function insertMove(Attribute $attribute, MoveEntity $move): MoveEntity
    {
        if ($move->amount()->value() < 0) {
            throw new AppException(ErrorCode::UNEXPECTED_AMOUNT, 'Move amount needs positive amount.');
        }

        $beforePurposeElementId = match (true) {
            $attribute->isPurpose() => $move->beforeId(),
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $afterPurposeElementId = match (true) {
            $attribute->isPurpose() => $move->afterId(),
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $beforePlaceElementId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => $move->beforeId(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $afterPlaceElementId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => $move->afterId(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        try {
            $beforeBalance = BalanceDataModel::insertBalance(
                (-1) * $move->amount()->value(),
                $move->item()->value(),
                KindElementId::moveId()->value(),
                $beforePurposeElementId->value(),
                $beforePlaceElementId->value(),
                $move->date()->value(),
                $move->groupId()?->value(),
            );

            $afterBalance = BalanceDataModel::insertBalance(
                $move->amount()->value(),
                $move->item()->value(),
                KindElementId::moveId()->value(),
                $afterPurposeElementId->value(),
                $afterPlaceElementId->value(),
                $move->date()->value(),
                $move->groupId()?->value() ?? $beforeBalance->id,
            );
        } catch (QueryException $e) {
            self::handleQueryException($e, 'Insert move error.');
        }

        // 1違いでなければ例外
        if ($afterBalance->id !== $beforeBalance->id + 1) {
            throw new AppException(ErrorCode::UNEXPECTED_DIFFERENCE_ID_MOVE, 'After id is wrong.');
        }

        if (is_null($beforeBalance->group_id)) {
            throw new AppException(ErrorCode::UNEXPECTED_NULL_READ, 'Group id is null.');
        }

        return new MoveEntity(
            MoveId::filledId($beforeBalance->id),
            new Amount($afterBalance->amount),
            new Item($beforeBalance->item),
            match (true) {
                $attribute->isPurpose() => PurposeElementId::filledId($beforeBalance->purpose_element_id),
                $attribute->isPlace() => PlaceElementId::filledId($beforeBalance->place_element_id),
                default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
            },
            match (true) {
                $attribute->isPurpose() => PurposeElementId::filledId($afterBalance->purpose_element_id),
                $attribute->isPlace() => PlaceElementId::filledId($afterBalance->place_element_id),
                default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
            },
            new Date($beforeBalance->date),
            GroupId::filledId($beforeBalance->group_id),
        );
    }

    public function updateMove(Attribute $attribute, MoveEntity $move): MoveEntity
    {
        if ($move->amount()->value() < 0) {
            throw new AppException(ErrorCode::UNEXPECTED_AMOUNT, 'Move amount needs positive amount.');
        }

        $beforePurposeElementId = match (true) {
            $attribute->isPurpose() => $move->beforeId(),
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $afterPurposeElementId = match (true) {
            $attribute->isPurpose() => $move->afterId(),
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $beforePlaceElementId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => $move->beforeId(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $afterPlaceElementId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => $move->afterId(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        try {
            $beforeBalance = BalanceDataModel::updateBalance(
                $move->moveId()->value(),
                (-1) * $move->amount()->value(),
                $move->item()->value(),
                KindElementId::moveId()->value(),
                $beforePurposeElementId->value(),
                $beforePlaceElementId->value(),
                $move->date()->value(),
                $move->groupId()?->value(),
            );

            $afterBalance = BalanceDataModel::updateBalance(
                $move->moveId()->value() + 1,
                $move->amount()->value(),
                $move->item()->value(),
                KindElementId::moveId()->value(),
                $afterPurposeElementId->value(),
                $afterPlaceElementId->value(),
                $move->date()->value(),
                $move->groupId()?->value(),
            );
        } catch (QueryException $e) {
            self::handleQueryException($e, 'Update move error.');
        }

        if (is_null($beforeBalance->group_id)) {
            throw new AppException(ErrorCode::UNEXPECTED_NULL_READ, 'Group id is null.');
        }

        return new MoveEntity(
            MoveId::filledId($beforeBalance->id),
            new Amount($afterBalance->amount),
            new Item($beforeBalance->item),
            match (true) {
                $attribute->isPurpose() => PurposeElementId::filledId($beforeBalance->purpose_element_id),
                $attribute->isPlace() => PlaceElementId::filledId($beforeBalance->place_element_id),
                default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
            },
            match (true) {
                $attribute->isPurpose() => PurposeElementId::filledId($afterBalance->purpose_element_id),
                $attribute->isPlace() => PlaceElementId::filledId($afterBalance->place_element_id),
                default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
            },
            new Date($beforeBalance->date),
            GroupId::filledId($beforeBalance->group_id),
        );
    }

    public function deleteMove(Attribute $attribute, MoveId $moveId): MoveEntity
    {
        try {
            $beforeBalance = BalanceDataModel::deleteBalance($moveId->value());
            $afterBalance = BalanceDataModel::deleteBalance($moveId->value() + 1);
        } catch (QueryException $e) {
            self::handleQueryException($e, 'Delete move error.');
        }

        if (is_null($beforeBalance->group_id)) {
            throw new AppException(ErrorCode::UNEXPECTED_NULL_READ, 'Group id is null.');
        }

        return new MoveEntity(
            MoveId::filledId($beforeBalance->id),
            new Amount($afterBalance->amount),
            new Item($beforeBalance->item),
            match (true) {
                $attribute->isPurpose() => PurposeElementId::filledId($beforeBalance->purpose_element_id),
                $attribute->isPlace() => PlaceElementId::filledId($beforeBalance->place_element_id),
                default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
            },
            match (true) {
                $attribute->isPurpose() => PurposeElementId::filledId($afterBalance->purpose_element_id),
                $attribute->isPlace() => PlaceElementId::filledId($afterBalance->place_element_id),
                default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
            },
            new Date($beforeBalance->date),
            is_null($beforeBalance->group_id) ? null : GroupId::filledId($beforeBalance->group_id),
        );
    }
}
