<?php

namespace App\Infrastructure\Repository\Impl;

use App\Domain\Entities\AttributeCategoryEntity;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\AttributeCategoryId;
use App\Domain\ValueObjects\AttributeCategoryName;
use App\Domain\ValueObjects\Description;
use App\Exceptions\InternalException;
use App\Infrastructure\Repository\AttributeCategoryRepositoryInterface;
use App\Models\DataModels\KindCategoryDataModel;
use App\Models\DataModels\PlaceCategoryDataModel;
use App\Models\DataModels\PurposeCategoryDataModel;

class AttributeCategoryRepositoryImpl implements AttributeCategoryRepositoryInterface
{
    public function getAttributeCategories(Attribute $attribute, ?AttributeCategoryId $attributeCategoryId = null): array
    {
        $attributeCategories = match (true) {
            $attribute->isKind() => KindCategoryDataModel::selectAttributeCategory(id: $attributeCategoryId?->value()),
            $attribute->isPurpose() => PurposeCategoryDataModel::selectAttributeCategory(id: $attributeCategoryId?->value()),
            $attribute->isPlace() => PlaceCategoryDataModel::selectAttributeCategory(id: $attributeCategoryId?->value()),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        return array_map(
            function ($attributeCategory) use ($attribute) {
                return new AttributeCategoryEntity(
                    $attribute,
                    AttributeCategoryId::filledId($attributeCategory->id),
                    new AttributeCategoryName($attributeCategory->name),
                    new Description($attributeCategory->description),
                );
            },
            $attributeCategories
        );
    }

    public function selectAttributeCategory(Attribute $attribute, AttributeCategoryId $attributeCategoryId): ?AttributeCategoryEntity
    {
        $attributeCategories = $this->getAttributeCategories(attribute: $attribute, attributeCategoryId: $attributeCategoryId);
        if (count($attributeCategories) === 0) {
            return null;
        }
        return $attributeCategories[0];
    }

    public function insertAttributeCategory(AttributeCategoryEntity $attributeCategory): int
    {
        return match (true) {
            $attributeCategory->attribute()->isKind() => KindCategoryDataModel::insertAttributeCategory(
                $attributeCategory->attributeCategoryName()->value(),
                $attributeCategory->description()->value(),
            ),
            $attributeCategory->attribute()->isPurpose() => PurposeCategoryDataModel::insertAttributeCategory(
                $attributeCategory->attributeCategoryName()->value(),
                $attributeCategory->description()->value(),
            ),
            $attributeCategory->attribute()->isPlace() => PlaceCategoryDataModel::insertAttributeCategory(
                $attributeCategory->attributeCategoryName()->value(),
                $attributeCategory->description()->value(),
            ),
            default => throw new InternalException('Attribute name is wrong.'),
        };
    }

    public function updateAttributeCategory(AttributeCategoryEntity $attributeCategory): void
    {
        match (true) {
            $attributeCategory->attribute()->isKind() => KindCategoryDataModel::updateAttributeCategory(
                $attributeCategory->attributeCategoryId()->value(),
                $attributeCategory->attributeCategoryName()->value(),
                $attributeCategory->description()->value(),
            ),
            $attributeCategory->attribute()->isPurpose() => PurposeCategoryDataModel::updateAttributeCategory(
                $attributeCategory->attributeCategoryId()->value(),
                $attributeCategory->attributeCategoryName()->value(),
                $attributeCategory->description()->value(),
            ),
            $attributeCategory->attribute()->isPlace() => PlaceCategoryDataModel::updateAttributeCategory(
                $attributeCategory->attributeCategoryId()->value(),
                $attributeCategory->attributeCategoryName()->value(),
                $attributeCategory->description()->value(),
            ),
            default => throw new InternalException('Attribute name is wrong.'),
        };
    }
}
