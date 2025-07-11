<?php

namespace App\Http\Controllers;

use App\Domain\Entities\AttributeCategoryEntity;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\AttributeCategoryId;
use App\Domain\ValueObjects\AttributeCategoryName;
use App\Domain\ValueObjects\Description;
use Illuminate\Http\Request;
use App\Exceptions\InvalidParameterException;
use App\Usecases\AttributeCategory\GetAttributeCategoriesUsecase;
use App\Usecases\AttributeCategory\InsertAttributeCategoryUsecase;
use App\Usecases\AttributeCategory\SelectAttributeCategoryUsecase;
use App\Usecases\AttributeCategory\UpdateAttributeCategoryUsecase;

class AttributeCategoryController extends Controller
{
    public function index(string $attributeName)
    {
        // 属性名
        $attribute = match ($attributeName) {
            'kind_category' => Attribute::kind(),
            'purpose_category' => Attribute::purpose(),
            'place_category' => Attribute::place(),
            default => throw new InvalidParameterException('Attribute name is wrong.'),
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
            default => throw new InvalidParameterException('Attribute name is wrong.'),
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
        // 属性名
        $attribute = match ($attributeName) {
            'kind_category' => Attribute::kind(),
            'purpose_category' => Attribute::purpose(),
            'place_category' => Attribute::place(),
            default => throw new InvalidParameterException('Attribute name is wrong.'),
        };

        $attributeCategory = new AttributeCategoryEntity(
            $attribute,
            AttributeCategoryId::emptyId(),
            new AttributeCategoryName($request->input('name')),
            new Description($request->input('description')),
        );

        $insertAttributeCategoryUsecase = new InsertAttributeCategoryUsecase();
        return $insertAttributeCategoryUsecase->execute($attributeCategory);
    }

    public function update(Request $request, string $attributeName, int $categoryId)
    {
        // 属性名
        $attribute = match ($attributeName) {
            'kind_category' => Attribute::kind(),
            'purpose_category' => Attribute::purpose(),
            'place_category' => Attribute::place(),
            default => throw new InvalidParameterException('Attribute name is wrong.'),
        };

        $attributeCategory = new AttributeCategoryEntity(
            $attribute,
            AttributeCategoryId::filledId($categoryId),
            new AttributeCategoryName($request->input('name')),
            new Description($request->input('description')),
        );

        $updateAttributeCategoryUsecase = new UpdateAttributeCategoryUsecase();
        return $updateAttributeCategoryUsecase->execute($attributeCategory);
    }
}
