<?php

namespace App\Infrastructure\Repository\Impl;

use App\Domain\Entities\AttributeElementEntity;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\AttributeCategoryId;
use App\Domain\ValueObjects\AttributeElementId;
use App\Domain\ValueObjects\AttributeElementName;
use App\Domain\ValueObjects\Description;
use App\Domain\ValueObjects\Priority;
use App\Exceptions\InternalException;
use App\Infrastructure\Repository\AttributeElementRepositoryInterface;
use App\Models\DataModels\KindElementDataModel;
use App\Models\DataModels\PlaceElementDataModel;
use App\Models\DataModels\PurposeElementDataModel;

class AttributeElementRepositoryImpl implements AttributeElementRepositoryInterface
{
    public function getAttributeElements(Attribute $attribute, ?AttributeElementId $attributeElementId = null): array
    {
        $attributeElements = match (true) {
            $attribute->isKind() => KindElementDataModel::selectAttributeElement(id: $attributeElementId?->value()),
            $attribute->isPurpose() => PurposeElementDataModel::selectAttributeElement(id: $attributeElementId?->value()),
            $attribute->isPlace() => PlaceElementDataModel::selectAttributeElement(id: $attributeElementId?->value()),
            default => throw new InternalException('Attribute name is wrong.'),
        };

        return array_map(
            function ($attributeElement) use ($attribute) {
                return new AttributeElementEntity(
                    $attribute,
                    AttributeElementId::filledId($attributeElement->id),
                    new AttributeElementName($attributeElement->name),
                    new Description($attributeElement->description),
                    new Priority($attributeElement->priority),
                    AttributeCategoryId::filledId($attributeElement->category_id),
                );
            },
            $attributeElements
        );
    }

    public function selectAttributeElement(Attribute $attribute, AttributeElementId $attributeElementId): ?AttributeElementEntity
    {
        $attributeElements = $this->getAttributeElements(attribute: $attribute, attributeElementId: $attributeElementId);
        if (count($attributeElements) === 0) {
            return null;
        }
        return $attributeElements[0];
    }

    public function insertAttributeElement(AttributeElementEntity $attributeElement): int
    {
        return match (true) {
            $attributeElement->attribute()->isKind() => KindElementDataModel::insertAttributeElement(
                $attributeElement->attributeElementName()->value(),
                $attributeElement->description()->value(),
                $attributeElement->priority()->value(),
                $attributeElement->attributeCategoryId()->value(),
            ),
            $attributeElement->attribute()->isPurpose() => PurposeElementDataModel::insertAttributeElement(
                $attributeElement->attributeElementName()->value(),
                $attributeElement->description()->value(),
                $attributeElement->priority()->value(),
                $attributeElement->attributeCategoryId()->value(),
            ),
            $attributeElement->attribute()->isPlace() => PlaceElementDataModel::insertAttributeElement(
                $attributeElement->attributeElementName()->value(),
                $attributeElement->description()->value(),
                $attributeElement->priority()->value(),
                $attributeElement->attributeCategoryId()->value(),
            ),
            default => throw new InternalException('Attribute name is wrong.'),
        };
    }

    public function updateAttributeElement(AttributeElementEntity $attributeElement): void
    {
        match (true) {
            $attributeElement->attribute()->isKind() => KindElementDataModel::updateAttributeElement(
                $attributeElement->attributeElementId()->value(),
                $attributeElement->attributeElementName()->value(),
                $attributeElement->description()->value(),
                $attributeElement->priority()->value(),
                $attributeElement->attributeCategoryId()->value(),
            ),
            $attributeElement->attribute()->isPurpose() => PurposeElementDataModel::updateAttributeElement(
                $attributeElement->attributeElementId()->value(),
                $attributeElement->attributeElementName()->value(),
                $attributeElement->description()->value(),
                $attributeElement->priority()->value(),
                $attributeElement->attributeCategoryId()->value(),
            ),
            $attributeElement->attribute()->isPlace() => PlaceElementDataModel::updateAttributeElement(
                $attributeElement->attributeElementId()->value(),
                $attributeElement->attributeElementName()->value(),
                $attributeElement->description()->value(),
                $attributeElement->priority()->value(),
                $attributeElement->attributeCategoryId()->value(),
            ),
            default => throw new InternalException('Attribute name is wrong.'),
        };
    }
}
