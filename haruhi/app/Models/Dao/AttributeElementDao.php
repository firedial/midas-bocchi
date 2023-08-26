<?php

namespace App\Models\Dao;

use Illuminate\Support\Facades\DB;

/**
 * attribute element を管理するための DAO
 */
Interface AttributeElementDao
{
    public function getAttributeElement(string $attributeName, bool $isOnlySelectable);
    public function getAttributeElementByElementId(string $attributeName, string $elementId);
    public function insertAttributeElement(string $attributeName, array $attributeElement);
    public function updateAttributeElement(string $attributeName, array $attributeElement);
}
