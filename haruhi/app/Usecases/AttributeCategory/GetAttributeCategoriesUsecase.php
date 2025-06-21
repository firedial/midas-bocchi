<?php

namespace App\Usecases\AttributeCategory;

use App\Domain\ValueObjects\Attribute;
use App\Infrastructure\Repository\AttributeCategoryRepositoryInterface;
use App\Infrastructure\Repository\Impl\AttributeCategoryRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class GetAttributeCategoriesUsecase
{
    private AttributeCategoryRepositoryImpl $attributeCategoryRepositoryImpl;

    public function __construct(?AttributeCategoryRepositoryInterface $attributeCategoryRepository = null)
    {
        $this->attributeCategoryRepositoryImpl = $attributeCategoryRepository ?: new AttributeCategoryRepositoryImpl();
    }

    public function execute(Attribute $attribute): array
    {
        DB::beginTransaction();
        try {
            $attributeCategories = $this->attributeCategoryRepositoryImpl->getAttributeCategories($attribute);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $attributeCategories;
    }
}
