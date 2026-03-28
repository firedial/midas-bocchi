<?php

namespace App\Http\Controllers;

use App\Domain\Entities\AttributeCategoryEntity;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\AttributeCategoryId;
use App\Domain\ValueObjects\AttributeCategoryName;
use App\Domain\ValueObjects\Description;
use Illuminate\Http\Request;
use App\Usecases\AttributeCategory\GetAttributeCategoriesUsecase;
use App\Usecases\AttributeCategory\InsertAttributeCategoryUsecase;
use App\Usecases\AttributeCategory\SelectAttributeCategoryUsecase;
use App\Usecases\AttributeCategory\UpdateAttributeCategoryUsecase;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;
use Illuminate\Validation\ValidationException;

class AttributeCategoryController extends Controller
{
    public function index(string $attributeName)
    {
        // 属性名
        $attribute = match ($attributeName) {
            'kind_category' => Attribute::kind(),
            'purpose_category' => Attribute::purpose(),
            'place_category' => Attribute::place(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $getAttributeCategoriesUsecase = new GetAttributeCategoriesUsecase();
        $attributeCategories = $getAttributeCategoriesUsecase->execute($attribute);

        return array_map(
            function (AttributeCategoryEntity $attributeCategory) {
                return [
                    "id" => $attributeCategory->attributeCategoryId()->value(),
                    "name" => $attributeCategory->attributeCategoryName()->value(),
                    "description" => $attributeCategory->description()->value(),
                ];
            },
            $attributeCategories
        );
    }

    public function show(string $attributeName, string $categoryId)
    {
        // 属性名
        $attribute = match ($attributeName) {
            'kind_category' => Attribute::kind(),
            'purpose_category' => Attribute::purpose(),
            'place_category' => Attribute::place(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $attributeCategoryId = AttributeCategoryId::filledId($categoryId);

        $selectAttributeCategoryUsecase = new SelectAttributeCategoryUsecase();
        $attributeCategory = $selectAttributeCategoryUsecase->execute($attribute, $attributeCategoryId);

        return [
            "id" => $attributeCategory->attributeCategoryId()->value(),
            "name" => $attributeCategory->attributeCategoryName()->value(),
            "description" => $attributeCategory->description()->value(),
        ];
    }

    public function store(Request $request, string $attributeName)
    {
        try {
            $validated = $request->validate([
                'name' => 'required|string',
                'description' => 'required|string',
            ]);
        } catch (ValidationException $e) {
            $failed = $e->validator->failed();

            foreach ($failed as $field => $rules) {
                if (isset($rules['Required'])) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "{$field} is required");
                }
                if (isset($rules['String'])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be a string type");
                }
            }

            throw $e;
        }

        // 属性名
        $attribute = match ($attributeName) {
            'kind_category' => Attribute::kind(),
            'purpose_category' => Attribute::purpose(),
            'place_category' => Attribute::place(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        $attributeCategory = new AttributeCategoryEntity(
            $attribute,
            AttributeCategoryId::emptyId(),
            new AttributeCategoryName($validated['name']),
            new Description($validated['description']),
        );

        $insertAttributeCategoryUsecase = new InsertAttributeCategoryUsecase();
        $result = $insertAttributeCategoryUsecase->execute($attributeCategory);
        return [
            "id" => $result->attributeCategoryId()->value(),
            "name" => $result->attributeCategoryName()->value(),
            "description" => $result->description()->value(),
        ];
    }

    public function update(Request $request, string $attributeName, int $categoryId)
    {
        try {
            $validated = $request->validate([
                'name' => 'required|string',
                'description' => 'required|string',
            ]);
        } catch (ValidationException $e) {
            $failed = $e->validator->failed();

            foreach ($failed as $field => $rules) {
                if (isset($rules['Required'])) {
                    throw new AppException(ErrorCode::MISSING_REQUIRED, "{$field} is required");
                }
                if (isset($rules['String'])) {
                    throw new AppException(ErrorCode::INVALID_TYPE, "{$field} must be a string type");
                }
            }

            throw $e;
        }

        // 属性名
        $attribute = match ($attributeName) {
            'kind_category' => Attribute::kind(),
            'purpose_category' => Attribute::purpose(),
            'place_category' => Attribute::place(),
            default => throw new AppException(ErrorCode::UNEXPECTED_ATTRIBUTE_NAME, 'Attribute name is wrong.'),
        };

        if (AttributeCategoryId::filledId($categoryId)->isMoveId()) {
            throw new AppException(ErrorCode::USING_MOVE_ID, 'Can not update category move id.');
        }

        $attributeCategory = new AttributeCategoryEntity(
            $attribute,
            AttributeCategoryId::filledId($categoryId),
            new AttributeCategoryName($validated['name']),
            new Description($validated['description']),
        );

        $updateAttributeCategoryUsecase = new UpdateAttributeCategoryUsecase();
        $result = $updateAttributeCategoryUsecase->execute($attributeCategory);
        return [
            "id" => $result->attributeCategoryId()->value(),
            "name" => $result->attributeCategoryName()->value(),
            "description" => $result->description()->value(),
        ];
    }
}
