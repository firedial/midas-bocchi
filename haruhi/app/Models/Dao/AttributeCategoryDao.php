<?php

namespace App\Models\Dao;

use Illuminate\Support\Facades\DB;

/**
 * attribute category を管理するための DAO
 */
interface AttributeCategoryDao
{
    public function getAttributeCategories(string $attributeName);
    public function insertAttributeCategory(string $attributeName, array $attributeCategory);
    public function updateAttributeCategory(string $attributeName, array $attributeCategory);
}
