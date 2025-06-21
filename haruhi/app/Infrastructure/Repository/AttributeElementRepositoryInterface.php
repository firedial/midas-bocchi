<?php

namespace App\Infrastructure\Repository;

use App\Domain\Entities\AttributeElementEntity;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\AttributeElementId;

interface AttributeElementRepositoryInterface
{
    public function getAttributeElements(Attribute $attribute, ?AttributeElementId $attributeElementId): array;
    public function selectAttributeElement(Attribute $attribute, AttributeElementId $attributeElementId): ?AttributeElementEntity;
    public function insertAttributeElement(AttributeElementEntity $attributeElement): int;
    public function updateAttributeElement(AttributeElementEntity $attributeElement): void;
}
