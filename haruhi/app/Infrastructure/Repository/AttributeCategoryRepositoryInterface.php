<?php

namespace App\Infrastructure\Repository;

use App\Domain\Entities\AttributeCategoryEntity;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\AttributeCategoryId;

interface AttributeCategoryRepositoryInterface
{
    public function getAttributeCategories(Attribute $attribute, ?AttributeCategoryId $attributeCategoryId): array;
    public function selectAttributeCategory(Attribute $attribute, AttributeCategoryId $attributeCategoryId): ?AttributeCategoryEntity;
    public function insertAttributeCategory(AttributeCategoryEntity $attributeCategory): int;
    public function updateAttributeCategory(AttributeCategoryEntity $attributeCategory): void;
}
