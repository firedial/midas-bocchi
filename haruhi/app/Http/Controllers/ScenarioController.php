<?php

namespace App\Http\Controllers;

use App\Domain\Entities\ScenarioDetailEntity;
use App\Domain\Entities\ScenarioEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\ScenarioId;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;
use App\Rules\StrictInteger;
use App\Usecases\Scenario\DeleteScenarioUsecase;
use App\Usecases\Scenario\GetScenariosUsecase;
use App\Usecases\Scenario\InsertScenarioUsecase;
use App\Usecases\Scenario\SelectScenarioUsecase;
use App\Usecases\Scenario\UpdateScenarioUsecase;
use Illuminate\Http\Request;
use Illuminate\Validation\ValidationException;

class ScenarioController extends Controller
{
    public function index()
    {
        $getScenariosUsecase = new GetScenariosUsecase();
        $scenarios = $getScenariosUsecase->execute();

        return array_map(
            fn (ScenarioEntity $s) => [
                'id' => $s->scenarioId()->value(),
                'name' => $s->name(),
            ],
            $scenarios
        );
    }

    public function show(int $id)
    {
        $selectScenarioUsecase = new SelectScenarioUsecase();
        $scenario = $selectScenarioUsecase->execute(ScenarioId::filledId($id));

        return [
            'id' => $scenario->scenarioId()->value(),
            'name' => $scenario->name(),
            'details' => array_map(
                fn (ScenarioDetailEntity $d) => [
                    'seq' => $d->seq(),
                    'type' => $d->type(),
                    'amount' => $d->amount()->value(),
                    'item' => $d->item()->value(),
                    'type_element_id' => $d->typeElementId(),
                    'purpose_element_id' => $d->purposeElementId(),
                    'place_element_id' => $d->placeElementId(),
                    'move_attribute' => $d->moveAttribute(),
                    'move_before_id' => $d->moveBeforeId(),
                    'move_after_id' => $d->moveAfterId(),
                ],
                $scenario->details()
            ),
        ];
    }

    public function store(Request $request)
    {
        [$name, $details] = $this->validateAndBuildDetails($request);

        $scenario = new ScenarioEntity(ScenarioId::emptyId(), $name, $details);

        $insertScenarioUsecase = new InsertScenarioUsecase();
        $result = $insertScenarioUsecase->execute($scenario);

        return [
            'id' => $result->scenarioId()->value(),
            'name' => $result->name(),
        ];
    }

    public function update(Request $request, int $id)
    {
        [$name, $details] = $this->validateAndBuildDetails($request);

        $scenario = new ScenarioEntity(ScenarioId::filledId($id), $name, $details);

        $updateScenarioUsecase = new UpdateScenarioUsecase();
        $result = $updateScenarioUsecase->execute($scenario);

        return [
            'id' => $result->scenarioId()->value(),
            'name' => $result->name(),
        ];
    }

    public function destroy(int $id)
    {
        $deleteScenarioUsecase = new DeleteScenarioUsecase();
        $result = $deleteScenarioUsecase->execute(ScenarioId::filledId($id));

        return [
            'id' => $result->scenarioId()->value(),
            'name' => $result->name(),
        ];
    }

    /**
     * リクエストをバリデートし、[name, ScenarioDetailEntity[]] を返す。
     */
    private function validateAndBuildDetails(Request $request): array
    {
        try {
            $validated = $request->validate([
                'name' => 'required|string|max:20',
                'details' => 'required|array|min:1',
                'details.*.type' => ['required', new StrictInteger, 'in:1,2'],
                'details.*.amount' => ['required', new StrictInteger],
                'details.*.item' => 'required|string|max:50',
                'details.*.type_element_id' => ['required', new StrictInteger],
            ]);
        } catch (ValidationException $e) {
            $failed = $e->validator->failed();

            foreach ($failed as $field => $rules) {
                if (isset($rules['Required'])) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "{$field} is required");
                }
                if (isset($rules[StrictInteger::class])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be an integer type");
                }
                if (isset($rules['String'])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be a string type");
                }
                if (isset($rules['In'])) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "{$field} is not a valid value");
                }
                if (isset($rules['Max'])) {
                    throw new AppException(ErrorCode::INVALID_LENGTH, "{$field} is too long");
                }
            }

            throw $e;
        }

        $detailEntities = [];
        foreach ($validated['details'] as $i => $detail) {
            $type = $detail['type'];
            $amount = $detail['amount'];
            $purposeElementId = $request->input("details.{$i}.purpose_element_id");
            $placeElementId = $request->input("details.{$i}.place_element_id");
            $moveAttribute = $request->input("details.{$i}.move_attribute");
            $moveBeforeId = $request->input("details.{$i}.move_before_id");
            $moveAfterId = $request->input("details.{$i}.move_after_id");

            if ($type === 1) {
                // 収支
                if ($amount === 0) {
                    throw new AppException(ErrorCode::INVALID_RANGE, "details.{$i}.amount must not be zero");
                }
                if (!is_int($purposeElementId)) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "details.{$i}.purpose_element_id is required");
                }
                if (!is_int($placeElementId)) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "details.{$i}.place_element_id is required");
                }
                if (!is_null($moveAttribute)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.move_attribute must be null");
                }
                if (!is_null($moveBeforeId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.move_before_id must be null");
                }
                if (!is_null($moveAfterId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.move_after_id must be null");
                }
            } else {
                // 移動 (type === 2)
                if ($amount <= 0) {
                    throw new AppException(ErrorCode::INVALID_RANGE, "details.{$i}.amount must be positive");
                }
                if (!is_null($purposeElementId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.purpose_element_id must be null");
                }
                if (!is_null($placeElementId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.place_element_id must be null");
                }
                if (!is_int($moveAttribute) || !in_array($moveAttribute, [1, 2], true)) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "details.{$i}.move_attribute is required (1 or 2)");
                }
                if (!is_int($moveBeforeId)) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "details.{$i}.move_before_id is required");
                }
                if (!is_int($moveAfterId)) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "details.{$i}.move_after_id is required");
                }
            }

            $detailEntities[] = new ScenarioDetailEntity(
                seq: $i + 1,
                type: $type,
                amount: new Amount($amount),
                item: new Item($detail['item']),
                typeElementId: $detail['type_element_id'],
                purposeElementId: $purposeElementId,
                placeElementId: $placeElementId,
                moveAttribute: $moveAttribute,
                moveBeforeId: $moveBeforeId,
                moveAfterId: $moveAfterId,
            );
        }

        return [$validated['name'], $detailEntities];
    }
}
