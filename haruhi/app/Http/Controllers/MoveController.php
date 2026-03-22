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
use App\Usecases\Move\DeleteMoveUsecase;
use App\Usecases\Move\GetMovesUsecase;
use App\Usecases\Move\InsertMoveUsecase;
use App\Usecases\Move\SelectMoveUsecase;
use App\Usecases\Move\UpdateMoveUsecase;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;
use App\Rules\StrictInteger;
use Illuminate\Validation\ValidationException;

class MoveController extends Controller
{

    public function index(Request $request, string $attributeName)
    {
        try {
            $validated = $request->validate([
                'limit' => ['int', 'min:1'],
            ]);
        } catch (ValidationException $e) {
            $failed = $e->validator->failed();

            foreach ($failed as $field => $rules) {
                if (isset($rules['Min'])) {
                    throw new AppException(ErrorCode::INVALID_RANGE, "{$field} must be at least 1");
                }
                if (isset($rules['Integer'])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be a int type");
                }
            }

            throw $e;
        }

        // 属性名
        $attribute = match ($attributeName) {
            'purposes' => Attribute::purpose(),
            'places' => Attribute::place(),
            default => throw new AppException(ErrorCode::INVALID_VALUE, 'Attribute name is wrong.'),
        };

        $getMovesUsecase = new GetMovesUsecase();
        $moves = $getMovesUsecase->execute($attribute, $validated['limit'] ?? null);

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
            throw new AppException(ErrorCode::INVALID_TYPE, 'Move id is null.');
        }

        // 属性名
        $attribute = match ($attributeName) {
            'purposes' => Attribute::purpose(),
            'places' => Attribute::place(),
            default => throw new AppException(ErrorCode::INVALID_VALUE, 'Attribute name is wrong.'),
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
        try {
            $validated = $request->validate([
                'amount' => ['required', new StrictInteger, 'integer', 'min:1'],
                'item' => 'required|string',
                'before_id' => ['required', new StrictInteger],
                'after_id' => ['required', new StrictInteger],
                'date' => 'required|string',
            ]);
        } catch (ValidationException $e) {
            $failed = $e->validator->failed();

            foreach ($failed as $field => $rules) {
                if (isset($rules[StrictInteger::class])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be an integer type");
                }
                if (isset($rules['Required'])) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "{$field} is required");
                }
                if (isset($rules['Min'])) {
                    throw new AppException(ErrorCode::INVALID_RANGE, "{$field} must be at least 1");
                }
                if (isset($rules['String'])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be a string type");
                }
            }

            throw $e;
        }

        // 属性名
        $attribute = match ($attributeName) {
            'purposes' => Attribute::purpose(),
            'places' => Attribute::place(),
            default => throw new AppException(ErrorCode::INVALID_VALUE, 'Attribute name is wrong.'),
        };

        // 移動前後で同じIDではないこと
        $beforeId = AttributeElementId::filledId($validated['before_id']);
        $afterId = AttributeElementId::filledId($validated['after_id']);
        if ($beforeId->value() === $afterId->value()) {
            throw new AppException(ErrorCode::MOVE_SAME_ID, 'Before id and after id is the same.');
        }

        // 移動IDではないこと
        $moveId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new AppException(ErrorCode::INVALID_VALUE, 'Attribute is wrong.'),
        };
        if ($beforeId->value() === $moveId->value() || $afterId->value() === $moveId->value()) {
            throw new AppException(ErrorCode::USING_MOVE_ID, 'Before id or after id is move id.');
        }

        $move = new MoveEntity(
            MoveId::emptyId(),
            new Amount($validated['amount']),
            new Item($validated['item']),
            $beforeId,
            $afterId,
            new Date($validated['date']),
        );

        $insertMoveUsecase = new InsertMoveUsecase();
        $result = $insertMoveUsecase->execute($attribute, $move);

        return [
            "id" => $result->moveId()->value(),
            "amount" => $result->amount()->value(),
            "item" => $result->item()->value(),
            "before_id" => $result->beforeId()->value(),
            "after_id" => $result->afterId()->value(),
            "date" => $result->date()->value(),
        ];
    }

    public function update(Request $request, string $attributeName, int $id)
    {
        try {
            $validated = $request->validate([
                'amount' => ['required', new StrictInteger, 'integer', 'min:1'],
                'item' => 'required|string',
                'before_id' => ['required', new StrictInteger],
                'after_id' => ['required', new StrictInteger],
                'date' => 'required|string',
            ]);
        } catch (ValidationException $e) {
            $failed = $e->validator->failed();

            foreach ($failed as $field => $rules) {
                if (isset($rules[StrictInteger::class])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be an integer type");
                }
                if (isset($rules['Required'])) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "{$field} is required");
                }
                if (isset($rules['Min'])) {
                    throw new AppException(ErrorCode::INVALID_RANGE, "{$field} must be at least 1");
                }
                if (isset($rules['String'])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be a string type");
                }
            }

            throw $e;
        }

        // 属性名
        $attribute = match ($attributeName) {
            'purposes' => Attribute::purpose(),
            'places' => Attribute::place(),
            default => throw new AppException(ErrorCode::INVALID_VALUE, 'Attribute name is wrong.'),
        };

        // 移動前後で同じIDではないこと
        $beforeId = AttributeElementId::filledId($request->input('before_id'));
        $afterId = AttributeElementId::filledId($request->input('after_id'));
        if ($beforeId->value() === $afterId->value()) {
            throw new AppException(ErrorCode::MOVE_SAME_ID, 'Before id and after id is the same.');
        }

        // 移動IDではないこと
        $moveId = match (true) {
            $attribute->isPurpose() => PurposeElementId::moveId(),
            $attribute->isPlace() => PlaceElementId::moveId(),
            default => throw new AppException(ErrorCode::INVALID_VALUE, 'Attribute is wrong.'),
        };
        if ($beforeId->value() === $moveId->value() || $afterId->value() === $moveId->value()) {
            throw new AppException(ErrorCode::USING_MOVE_ID, 'Before id or after id is move id.');
        }

        $move = new MoveEntity(
            MoveId::filledId($id),
            new Amount($validated['amount']),
            new Item($validated['item']),
            $beforeId,
            $afterId,
            new Date($validated['date']),
        );

        $updateMoveUsecase = new UpdateMoveUsecase();
        $result = $updateMoveUsecase->execute($attribute, $move);

        return [
            "id" => $result->moveId()->value(),
            "amount" => $result->amount()->value(),
            "item" => $result->item()->value(),
            "before_id" => $result->beforeId()->value(),
            "after_id" => $result->afterId()->value(),
            "date" => $result->date()->value(),
        ];
    }

    public function destroy(string $attributeName, int $id)
    {
        // 属性名
        $attribute = match ($attributeName) {
            'purposes' => Attribute::purpose(),
            'places' => Attribute::place(),
            default => throw new AppException(ErrorCode::INVALID_VALUE, 'Attribute name is wrong.'),
        };

        $deleteMoveUsecase = new DeleteMoveUsecase();
        $result = $deleteMoveUsecase->execute($attribute, MoveId::filledId($id));

        return [
            "id" => $result->moveId()->value(),
            "amount" => $result->amount()->value(),
            "item" => $result->item()->value(),
            "before_id" => $result->beforeId()->value(),
            "after_id" => $result->afterId()->value(),
            "date" => $result->date()->value(),
        ];
    }
}
