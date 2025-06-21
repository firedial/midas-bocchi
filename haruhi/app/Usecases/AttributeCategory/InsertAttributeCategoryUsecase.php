<?php

namespace App\Usecases\AttributeCategory;

use App\Domain\Entities\AttributeCategoryEntity;
use App\Infrastructure\Repository\AttributeCategoryRepositoryInterface;
use App\Infrastructure\Repository\Impl\AttributeCategoryRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class InsertAttributeCategoryUsecase
{
    private AttributeCategoryRepositoryImpl $attributeCategoryRepositoryImpl;

    public function __construct(?AttributeCategoryRepositoryInterface $attributeCategoryRepository = null)
    {
        $this->attributeCategoryRepositoryImpl = $attributeCategoryRepository ?: new AttributeCategoryRepositoryImpl();
    }

    public function execute(AttributeCategoryEntity $attributeCategory): int
    {
        DB::beginTransaction();
        try {
            // 挿入
            $insertId = $this->attributeCategoryRepositoryImpl->insertAttributeCategory($attributeCategory);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $insertId;
    }
}
