<?php

namespace App\Http\Controllers;

use App\Domain\Entities\BalanceEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\BalanceId;
use App\Domain\ValueObjects\GroupId;
use Illuminate\Http\Request;
use App\Domain\ValueObjects\Date;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Usecases\Balance\DeleteBalanceUsecase;
use App\Usecases\Balance\GetBalancesUsecase;
use App\Usecases\Balance\InsertBalanceUsecase;
use App\Usecases\Balance\SelectBalanceUsecase;
use App\Usecases\Balance\UpdateBalanceUsecase;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;
use App\Rules\StrictInteger;
use Illuminate\Validation\ValidationException;

class BalanceController extends Controller
{
    public function index(Request $request)
    {
        try {
            $validated = $request->validate([
                'limit' => ['int', 'min:1'],
                'orderby' => ['string', 'in:desc'],
            ]);
        } catch (ValidationException $e) {
            $failed = $e->validator->failed();

            foreach ($failed as $field => $rules) {
                // StrictInteger はクラス名で入る
                if (isset($rules[StrictInteger::class])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be an integer type");
                }
                if (isset($rules['Min'])) {
                    throw new AppException(ErrorCode::INVALID_RANGE, "{$field} must be at least 1");
                }
                if (isset($rules['String'])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be a string type");
                }
                if (isset($rules['Integer'])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be a int type");
                }
                if (isset($rules['In'])) {
                    throw new AppException(ErrorCode::INVALID_FORMAT, "{$field} is not a valid value");
                }
            }

            throw $e;
        }

        $getBalancesUsecase = new GetBalancesUsecase();
        $balances = $getBalancesUsecase->execute($validated['limit'] ?? null, !array_key_exists('orderby', $validated) ? null : true);
        return array_map(
            function (BalanceEntity $balance) {
                return [
                    "id" => $balance->balanceId()->value(),
                    "amount" => $balance->amount()->value(),
                    "item" => $balance->item()->value(),
                    "kind_element_id" => $balance->kindElementId()->value(),
                    "purpose_element_id" => $balance->purposeElementId()->value(),
                    "place_element_id" => $balance->placeElementId()->value(),
                    "group_id" => $balance->groupId()?->value(),
                    "kind_element_description" => $balance->kindElementDescription()->value(),
                    "purpose_element_description" => $balance->purposeElementDescription()->value(),
                    "place_element_description" => $balance->placeElementDescription()->value(),
                    "date" => $balance->date()->value(),
                ];
            },
            $balances
        );
    }

    public function show(int $id)
    {
        $balanceId = BalanceId::filledId($id);

        $selectBalanceUsecase = new SelectBalanceUsecase();
        $balance = $selectBalanceUsecase->execute($balanceId);
        return [
            "id" => $balance->balanceId()->value(),
            "amount" => $balance->amount()->value(),
            "item" => $balance->item()->value(),
            "kind_element_id" => $balance->kindElementId()->value(),
            "purpose_element_id" => $balance->purposeElementId()->value(),
            "place_element_id" => $balance->placeElementId()->value(),
            "group_id" => $balance->groupId()?->value(),
            "kind_element_description" => $balance->kindElementDescription()->value(),
            "purpose_element_description" => $balance->purposeElementDescription()->value(),
            "place_element_description" => $balance->placeElementDescription()->value(),
            "date" => $balance->date()->value(),
        ];
    }

    public function store(Request $request)
    {
        try {
            $validated = $request->validate([
                'amount' => ['present', new StrictInteger],
                'kind_element_id' => ['present', new StrictInteger],
                'purpose_element_id' => ['present', new StrictInteger],
                'place_element_id' => ['present', new StrictInteger],
                'item' => 'present|string',
                'date' => 'present|string',
                'group_id' => ['nullable', new StrictInteger(nullable: true), 'min:1'],
            ]);
        } catch (ValidationException $e) {
            $failed = $e->validator->failed();

            foreach ($failed as $field => $rules) {
                if (isset($rules['Present'])) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "{$field} is required");
                }
                // StrictInteger はクラス名で入る
                if (isset($rules[StrictInteger::class])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be an integer type");
                }
                if (isset($rules['String'])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be a string type");
                }
                if (isset($rules['Min'])) {
                    throw new AppException(ErrorCode::INVALID_RANGE, "{$field} must be at least 1");
                }
            }

            throw $e;
        }

        $groupId = !is_null($validated['group_id'] ?? null) ? GroupId::filledId($validated['group_id']) : null;

        $balance = new BalanceEntity(
            BalanceId::emptyId(),
            new Amount($validated['amount']),
            new Item($validated['item']),
            KindElementId::filledId($validated['kind_element_id']),
            PurposeElementId::filledId($validated['purpose_element_id']),
            PlaceElementId::filledId($validated['place_element_id']),
            new Date($validated['date']),
            $groupId,
        );

        if ($balance->kindElementId()->isMoveId()) {
            throw new AppException(ErrorCode::USING_MOVE_ID, 'Kind element id is move id.');
        }

        if ($balance->purposeElementId()->isMoveId()) {
            throw new AppException(ErrorCode::USING_MOVE_ID, 'Purpose element id is move id.');
        }

        if ($balance->placeElementId()->isMoveId()) {
            throw new AppException(ErrorCode::USING_MOVE_ID, 'Place element id is move id.');
        }

        if ($balance->amount()->value() === 0) {
            throw new AppException(ErrorCode::INVALID_RANGE, 'Amount is zero.');
        }

        $insertBalanceUsecase = new InsertBalanceUsecase();
        $result = $insertBalanceUsecase->execute($balance);
        return [
            "id" => $result->balanceId()->value(),
            "amount" => $result->amount()->value(),
            "item" => $result->item()->value(),
            "kind_element_id" => $result->kindElementId()->value(),
            "purpose_element_id" => $result->purposeElementId()->value(),
            "place_element_id" => $result->placeElementId()->value(),
            "group_id" => $result->groupId()?->value(),
            "date" => $result->date()->value(),
        ];
    }

    public function update(Request $request, int $id)
    {
        try {
            $validated = $request->validate([
                'amount' => ['present', new StrictInteger],
                'kind_element_id' => ['present', new StrictInteger],
                'purpose_element_id' => ['present', new StrictInteger],
                'place_element_id' => ['present', new StrictInteger],
                'item' => 'present|string',
                'date' => 'present|string',
                'group_id' => ['present', new StrictInteger, 'min:1'],
            ]);
        } catch (ValidationException $e) {
            $failed = $e->validator->failed();

            foreach ($failed as $field => $rules) {
                if (isset($rules['Present'])) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "{$field} is required");
                }
                if (isset($rules[StrictInteger::class])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be an integer type");
                }
                if (isset($rules['String'])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be a string type");
                }
                if (isset($rules['Min'])) {
                    throw new AppException(ErrorCode::INVALID_RANGE, "{$field} must be at least 1");
                }
            }

            throw $e;
        }

        $groupId = GroupId::filledId($validated['group_id']);

        $balance = new BalanceEntity(
            BalanceId::filledId($id),
            new Amount($validated['amount']),
            new Item($validated['item']),
            KindElementId::filledId($validated['kind_element_id']),
            PurposeElementId::filledId($validated['purpose_element_id']),
            PlaceElementId::filledId($validated['place_element_id']),
            new Date($validated['date']),
            $groupId,
        );

        if ($balance->kindElementId()->isMoveId()) {
            throw new AppException(ErrorCode::USING_MOVE_ID, 'Kind element id is move id.');
        }

        if ($balance->purposeElementId()->isMoveId()) {
            throw new AppException(ErrorCode::USING_MOVE_ID, 'Purpose element id is move id.');
        }

        if ($balance->placeElementId()->isMoveId()) {
            throw new AppException(ErrorCode::USING_MOVE_ID, 'Place element id is move id.');
        }

        if ($balance->amount()->value() === 0) {
            throw new AppException(ErrorCode::INVALID_RANGE, 'Amount is zero.');
        }

        $updateBalanceUsecase = new UpdateBalanceUsecase();
        $result = $updateBalanceUsecase->execute($balance);

        return [
            "id" => $result->balanceId()->value(),
            "amount" => $result->amount()->value(),
            "item" => $result->item()->value(),
            "kind_element_id" => $result->kindElementId()->value(),
            "purpose_element_id" => $result->purposeElementId()->value(),
            "place_element_id" => $result->placeElementId()->value(),
            "group_id" => $result->groupId()?->value(),
            "date" => $result->date()->value(),
        ];
    }

    public function destroy(int $id)
    {
        $balanceId = BalanceId::filledId($id);

        $deleteBalanceUsecase = new DeleteBalanceUsecase();
        $result = $deleteBalanceUsecase->execute($balanceId);

        return [
            "id" => $result->balanceId()->value(),
            "amount" => $result->amount()->value(),
            "item" => $result->item()->value(),
            "kind_element_id" => $result->kindElementId()->value(),
            "purpose_element_id" => $result->purposeElementId()->value(),
            "place_element_id" => $result->placeElementId()->value(),
            "group_id" => $result->groupId()?->value(),
            "date" => $result->date()->value(),
        ];
    }
}
