<?php

namespace App\Http\Controllers;

use App\Domain\Entities\MoveEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\AttributeElementId;
use App\Domain\ValueObjects\Date;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\MoveId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\PurposeElementId;
use Illuminate\Http\Request;
use App\Exceptions\InvalidParameterException;
use App\Usecases\Move\DeleteMoveUsecase;
use App\Usecases\Move\GetMovesUsecase;
use App\Usecases\Move\InsertMoveUsecase;
use App\Usecases\Move\SelectMoveUsecase;
use App\Usecases\Move\UpdateMoveUsecase;

class MoveController extends Controller
{

    public function index(Request $request, string $attributeName)
    {
        $limit = $request->input('limit');
        if (!is_null($limit) && !is_numeric($limit)) {
            throw new InvalidParameterException('limit is wrong');
        }

        // 属性名
        $attribute = match ($attributeName) {
            'purposes' => Attribute::purpose(),
            'places' => Attribute::place(),
            default => throw new InvalidParameterException('Attribute name is wrong.'),
        };

        $getMovesUsecase = new GetMovesUsecase();
        $moves = $getMovesUsecase->execute($attribute, $limit);

        return array_map(
            function (MoveEntity $move) {
                return [
                    "id" => $move->moveId()->value(),
                    "amount" => $move->amount()->value(),
                    "item" => $move->item()->value(),
                    "before_id" => $move->beforeId()->value(),
                    "after_id" => $move->afterId()->value(),
                    "date" => $move->date()->value(),
                    "before_description" => $move->beforeDescription()->value(),
                    "after_description" => $move->afterDescription()->value(),
                ];
            },
            $moves
        );
    }

    public function show(string $attributeName, int $id)
    {
        if (is_null($id) || !is_numeric($id)) {
            throw new InvalidParameterException('Move id is null.');
        }

        // 属性名
        $attribute = match ($attributeName) {
            'purposes' => Attribute::purpose(),
            'places' => Attribute::place(),
            default => throw new InvalidParameterException('Attribute name is wrong.'),
        };

        $SelectMoveUsecase = new SelectMoveUsecase();
        $move = $SelectMoveUsecase->execute($attribute, MoveId::filledId($id));

        return [
            "id" => $move->moveId()->value(),
            "amount" => $move->amount()->value(),
            "item" => $move->item()->value(),
            "before_id" => $move->beforeId()->value(),
            "after_id" => $move->afterId()->value(),
            "date" => $move->date()->value(),
            "before_description" => $move->beforeDescription()->value(),
            "after_description" => $move->afterDescription()->value(),
        ];
    }

    public function store(Request $request, string $attributeName)
    {
        // 属性名
        $attribute = match ($attributeName) {
            'purposes' => Attribute::purpose(),
            'places' => Attribute::place(),
            default => throw new InvalidParameterException('Attribute name is wrong.'),
        };

        // 金額は正である必要がある
        $amount = new Amount($request->input('amount'));
        if ($amount->value() <= 0) {
            throw new InvalidParameterException('Amount is zero or minus.');
        }

        // 移動前後で同じIDではないこと
        $beforeId = AttributeElementId::filledId($request->input('before_id'));
        $afterId = AttributeElementId::filledId($request->input('after_id'));
        if ($beforeId->value() === $afterId->value()) {
            throw new InvalidParameterException('Before id and after id is the same.');
        }

        // 移動IDではないこと
        $moveId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new InvalidParameterException('Attribute is wrong.'),
        };
        if ($beforeId->value() === $moveId->value() || $afterId->value() === $moveId->value()) {
            throw new InvalidParameterException('Before id or after id is move id.');
        }

        $move = new MoveEntity(
            MoveId::emptyId(),
            $amount,
            new Item($request->input('item')),
            $beforeId,
            $afterId,
            new Date($request->input('date')),
        );

        $insertMoveUsecase = new InsertMoveUsecase();
        return $insertMoveUsecase->execute($attribute, $move);
    }

    public function update(Request $request, string $attributeName, int $id)
    {
        // 属性名
        $attribute = match ($attributeName) {
            'purposes' => Attribute::purpose(),
            'places' => Attribute::place(),
            default => throw new InvalidParameterException('Attribute name is wrong.'),
        };

        // 金額は正である必要がある
        $amount = new Amount($request->input('amount'));
        if ($amount->value() <= 0) {
            throw new InvalidParameterException('Amount is zero or minus.');
        }

        // 移動前後で同じIDではないこと
        $beforeId = AttributeElementId::filledId($request->input('before_id'));
        $afterId = AttributeElementId::filledId($request->input('after_id'));
        if ($beforeId->value() === $afterId->value()) {
            throw new InvalidParameterException('Before id and after id is the same.');
        }

        // 移動IDではないこと
        $moveId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new InvalidParameterException('Attribute is wrong.'),
        };
        if ($beforeId->value() === $moveId->value() || $afterId->value() === $moveId->value()) {
            throw new InvalidParameterException('Before id or after id is move id.');
        }

        $move = new MoveEntity(
            MoveId::filledId($id),
            $amount,
            new Item($request->input('item')),
            $beforeId,
            $afterId,
            new Date($request->input('date')),
        );

        $updateMoveUsecase = new UpdateMoveUsecase();
        return $updateMoveUsecase->execute($attribute, $move);
    }

    public function destroy(string $attributeName, int $id)
    {
        // 属性名
        $attribute = match ($attributeName) {
            'purposes' => Attribute::purpose(),
            'places' => Attribute::place(),
            default => throw new InvalidParameterException('Attribute name is wrong.'),
        };

        $deleteMoveUsecase = new DeleteMoveUsecase();
        return $deleteMoveUsecase->execute($attribute, MoveId::filledId($id));
    }
}
