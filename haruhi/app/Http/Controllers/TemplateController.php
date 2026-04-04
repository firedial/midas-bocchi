<?php

namespace App\Http\Controllers;

use App\Domain\Entities\TemplateDetailEntity;
use App\Domain\Entities\TemplateEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\TemplateId;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;
use App\Rules\StrictInteger;
use App\Usecases\Template\DeleteTemplateUsecase;
use App\Usecases\Template\GetTemplatesUsecase;
use App\Usecases\Template\InsertTemplateUsecase;
use App\Usecases\Template\SelectTemplateUsecase;
use App\Usecases\Template\UpdateTemplateUsecase;
use Illuminate\Http\Request;
use Illuminate\Validation\ValidationException;

class TemplateController extends Controller
{
    public function index()
    {
        $getTemplatesUsecase = new GetTemplatesUsecase();
        $templates = $getTemplatesUsecase->execute();

        return array_map(
            fn (TemplateEntity $s) => [
                'id' => $s->templateId()->value(),
                'name' => $s->name(),
            ],
            $templates
        );
    }

    public function show(int $id)
    {
        $selectTemplateUsecase = new SelectTemplateUsecase();
        $template = $selectTemplateUsecase->execute(TemplateId::filledId($id));

        return [
            'id' => $template->templateId()->value(),
            'name' => $template->name(),
            'details' => array_map(
                fn (TemplateDetailEntity $d) => [
                    'seq' => $d->seq(),
                    'type' => $d->type(),
                    'amount' => $d->amount()->value(),
                    'item' => $d->item()->value(),
                    'kind_element_id' => $d->kindElementId(),
                    'purpose_element_id' => $d->purposeElementId(),
                    'place_element_id' => $d->placeElementId(),
                    'move_before_purpose_id' => $d->moveBeforePurposeId(),
                    'move_after_purpose_id' => $d->moveAfterPurposeId(),
                    'move_before_place_id' => $d->moveBeforePlaceId(),
                    'move_after_place_id' => $d->moveAfterPlaceId(),
                ],
                $template->details()
            ),
        ];
    }

    public function store(Request $request)
    {
        [$name, $details] = $this->validateAndBuildDetails($request);

        $template = new TemplateEntity(TemplateId::emptyId(), $name, $details);

        $insertTemplateUsecase = new InsertTemplateUsecase();
        $result = $insertTemplateUsecase->execute($template);

        return [
            'id' => $result->templateId()->value(),
            'name' => $result->name(),
        ];
    }

    public function update(Request $request, int $id)
    {
        [$name, $details] = $this->validateAndBuildDetails($request);

        $template = new TemplateEntity(TemplateId::filledId($id), $name, $details);

        $updateTemplateUsecase = new UpdateTemplateUsecase();
        $result = $updateTemplateUsecase->execute($template);

        return [
            'id' => $result->templateId()->value(),
            'name' => $result->name(),
        ];
    }

    public function destroy(int $id)
    {
        $deleteTemplateUsecase = new DeleteTemplateUsecase();
        $result = $deleteTemplateUsecase->execute(TemplateId::filledId($id));

        return [
            'id' => $result->templateId()->value(),
            'name' => $result->name(),
        ];
    }

    /**
     * リクエストをバリデートし、[name, TemplateDetailEntity[]] を返す。
     */
    private function validateAndBuildDetails(Request $request): array
    {
        try {
            $validated = $request->validate([
                'name' => 'required|string|max:20',
                'details' => 'required|array|min:1',
                'details.*.type' => ['required', new StrictInteger, 'in:1,2,3'],
                'details.*.amount' => ['required', new StrictInteger],
                'details.*.item' => 'required|string|max:50',
                'details.*.kind_element_id' => ['required', new StrictInteger],
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
                if (isset($rules['Min'])) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "{$field} must have at least one item");
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
            $moveBeforePurposeId = $request->input("details.{$i}.move_before_purpose_id");
            $moveAfterPurposeId = $request->input("details.{$i}.move_after_purpose_id");
            $moveBeforePlaceId = $request->input("details.{$i}.move_before_place_id");
            $moveAfterPlaceId = $request->input("details.{$i}.move_after_place_id");

            if ($type === 1) {
                // 収支
                if (!is_int($purposeElementId)) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "details.{$i}.purpose_element_id is required");
                }
                if (!is_int($placeElementId)) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "details.{$i}.place_element_id is required");
                }
                if (!is_null($moveBeforePurposeId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.move_before_purpose_id must be null");
                }
                if (!is_null($moveAfterPurposeId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.move_after_purpose_id must be null");
                }
                if (!is_null($moveBeforePlaceId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.move_before_place_id must be null");
                }
                if (!is_null($moveAfterPlaceId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.move_after_place_id must be null");
                }
            } elseif ($type === 2) {
                // 予算移動
                if ($amount <= 0) {
                    throw new AppException(ErrorCode::INVALID_RANGE, "details.{$i}.amount must be positive");
                }
                if (!is_null($purposeElementId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.purpose_element_id must be null");
                }
                if (!is_null($placeElementId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.place_element_id must be null");
                }
                if (!is_int($moveBeforePurposeId)) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "details.{$i}.move_before_purpose_id is required");
                }
                if (!is_int($moveAfterPurposeId)) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "details.{$i}.move_after_purpose_id is required");
                }
                if (!is_null($moveBeforePlaceId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.move_before_place_id must be null");
                }
                if (!is_null($moveAfterPlaceId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.move_after_place_id must be null");
                }
            } else {
                // 場所移動 (type === 3)
                if ($amount <= 0) {
                    throw new AppException(ErrorCode::INVALID_RANGE, "details.{$i}.amount must be positive");
                }
                if (!is_null($purposeElementId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.purpose_element_id must be null");
                }
                if (!is_null($placeElementId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.place_element_id must be null");
                }
                if (!is_null($moveBeforePurposeId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.move_before_purpose_id must be null");
                }
                if (!is_null($moveAfterPurposeId)) {
                    throw new AppException(ErrorCode::INVALID_VALUE, "details.{$i}.move_after_purpose_id must be null");
                }
                if (!is_int($moveBeforePlaceId)) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "details.{$i}.move_before_place_id is required");
                }
                if (!is_int($moveAfterPlaceId)) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "details.{$i}.move_after_place_id is required");
                }
            }

            $detailEntities[] = new TemplateDetailEntity(
                seq: $i + 1,
                type: $type,
                amount: new Amount($amount),
                item: new Item($detail['item']),
                kindElementId: $detail['kind_element_id'],
                purposeElementId: $purposeElementId,
                placeElementId: $placeElementId,
                moveBeforePurposeId: $moveBeforePurposeId,
                moveAfterPurposeId: $moveAfterPurposeId,
                moveBeforePlaceId: $moveBeforePlaceId,
                moveAfterPlaceId: $moveAfterPlaceId,
            );
        }

        return [$validated['name'], $detailEntities];
    }
}
