<?php

namespace App\Http\Controllers;

use App\Domain\Entities\AttributeElementEntity;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\AttributeElementId;
use App\Domain\ValueObjects\AttributeElementName;
use App\Domain\ValueObjects\Description;
use App\Domain\ValueObjects\KindCategoryId;
use App\Domain\ValueObjects\PlaceCategoryId;
use App\Domain\ValueObjects\Priority;
use App\Domain\ValueObjects\PurposeCategoryId;
use Illuminate\Http\Request;
use App\Usecases\AttributeElement\GetAttributeElementsUsecase;
use App\Usecases\AttributeElement\InsertAttributeElementUsecase;
use App\Usecases\AttributeElement\SelectAttributeElementUsecase;
use App\Usecases\AttributeElement\UpdateAttributeElementUsecase;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;
use App\Rules\StrictInteger;
use Illuminate\Validation\ValidationException;

class AttributeElementController extends Controller
{
    public function index(string $attributeName)
    {
        // 属性名
        $attribute = match ($attributeName) {
            'kind_element' => Attribute::kind(),
            'purpose_element' => Attribute::purpose(),
            'place_element' => Attribute::place(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $getAttributeElementsUsecase = new GetAttributeElementsUsecase();
        $attributeElements = $getAttributeElementsUsecase->execute($attribute);

        return array_map(
            function (AttributeElementEntity $attributeElement) {
                return [
                    "id" => $attributeElement->attributeElementId()->value(),
                    "name" => $attributeElement->attributeElementName()->value(),
                    "description" => $attributeElement->description()->value(),
                    "priority" => $attributeElement->priority()->value(),
                    "category_id" => $attributeElement->attributeCategoryId()->value(),
                ];
            },
            $attributeElements
        );
    }

    public function show(string $attributeName, string $elementId)
    {
        // 属性名
        $attribute = match ($attributeName) {
            'kind_element' => Attribute::kind(),
            'purpose_element' => Attribute::purpose(),
            'place_element' => Attribute::place(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $attributeElementId = AttributeElementId::filledId($elementId);

        $selectAttributeElementUsecase = new SelectAttributeElementUsecase();
        $attributeElement = $selectAttributeElementUsecase->execute($attribute, $attributeElementId);

        return [
            "id" => $attributeElement->attributeElementId()->value(),
            "name" => $attributeElement->attributeElementName()->value(),
            "description" => $attributeElement->description()->value(),
            "priority" => $attributeElement->priority()->value(),
            "category_id" => $attributeElement->attributeCategoryId()->value(),
        ];
    }

    public function store(Request $request, string $attributeName)
    {
        try {
            $validated = $request->validate([
                'name' => 'required|string',
                'description' => 'required|string',
                'priority' => ['required', new StrictInteger],
                'category_id' => ['required', new StrictInteger],
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
            }

            throw $e;
        }

        // 属性名
        $attribute = match ($attributeName) {
            'kind_element' => Attribute::kind(),
            'purpose_element' => Attribute::purpose(),
            'place_element' => Attribute::place(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $attributeCategoryId = match (true) {
            $attribute->isKind() => KindCategoryId::filledId($request->input('category_id')),
            $attribute->isPurpose() => PurposeCategoryId::filledId($request->input('category_id')),
            $attribute->isPlace() => PlaceCategoryId::filledId($request->input('category_id')),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        if ($attributeCategoryId->isMoveId()) {
            throw new AppException(ErrorCode::USING_MOVE_ID, 'Attribute category id is move id.');
        }

        $attributeElement = new AttributeElementEntity(
            $attribute,
            AttributeElementId::emptyId(),
            new AttributeElementName($validated['name']),
            new Description($validated['description']),
            new Priority($validated['priority']),
            $attributeCategoryId,
        );

        $insertAttributeElementUsecase = new InsertAttributeElementUsecase();
        $result = $insertAttributeElementUsecase->execute($attributeElement);
        return [
            "id" => $result->attributeElementId()->value(),
            "name" => $result->attributeElementName()->value(),
            "description" => $result->description()->value(),
            "priority" => $result->priority()->value(),
            "category_id" => $result->attributeCategoryId()->value(),
        ];
    }

    public function update(Request $request, string $attributeName, int $elementId)
    {
        try {
            $validated = $request->validate([
                'name' => 'required|string',
                'description' => 'required|string',
                'priority' => ['required', new StrictInteger],
                'category_id' => ['required', new StrictInteger],
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
            }

            throw $e;
        }

        // 属性名
        $attribute = match ($attributeName) {
            'kind_element' => Attribute::kind(),
            'purpose_element' => Attribute::purpose(),
            'place_element' => Attribute::place(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $attributeCategoryId = match (true) {
            $attribute->isKind() => KindCategoryId::filledId($request->input('category_id')),
            $attribute->isPurpose() => PurposeCategoryId::filledId($request->input('category_id')),
            $attribute->isPlace() => PlaceCategoryId::filledId($request->input('category_id')),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        if (AttributeElementId::filledId($elementId)->isMoveId()) {
            throw new AppException(ErrorCode::USING_MOVE_ID, 'Can not update element move id.');
        }

        if ($attributeCategoryId->isMoveId()) {
            throw new AppException(ErrorCode::USING_MOVE_ID, 'Attribute category id is move id.');
        }

        $attributeElement = new AttributeElementEntity(
            $attribute,
            AttributeElementId::filledId($elementId),
            new AttributeElementName($validated['name']),
            new Description($validated['description']),
            new Priority($validated['priority']),
            $attributeCategoryId,
        );

        $updateAttributeElementUsecase = new UpdateAttributeElementUsecase();
        $result = $updateAttributeElementUsecase->execute($attributeElement);
        return [
            "id" => $result->attributeElementId()->value(),
            "name" => $result->attributeElementName()->value(),
            "description" => $result->description()->value(),
            "priority" => $result->priority()->value(),
            "category_id" => $result->attributeCategoryId()->value(),
        ];
    }
}
