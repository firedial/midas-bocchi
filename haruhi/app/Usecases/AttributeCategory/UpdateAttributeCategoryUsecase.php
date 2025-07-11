<?php

namespace App\Usecases\AttributeCategory;

use App\Domain\Entities\AttributeCategoryEntity;
use App\Exceptions\NotFoundException;
use App\Infrastructure\Repository\AttributeCategoryRepositoryInterface;
use App\Infrastructure\Repository\Impl\AttributeCategoryRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class UpdateAttributeCategoryUsecase
{
    private AttributeCategoryRepositoryImpl $attributeCategoryRepositoryImpl;

    public function __construct(?AttributeCategoryRepositoryInterface $attributeCategoryRepository = null)
    {
        $this->attributeCategoryRepositoryImpl = $attributeCategoryRepository ?: new AttributeCategoryRepositoryImpl();
    }

    public function execute(AttributeCategoryEntity $attributeCategory): void
    {
        DB::beginTransaction();
        try {
            $beforeAttributeCategory = $this->attributeCategoryRepositoryImpl->selectAttributeCategory($attributeCategory->attribute(), $attributeCategory->attributeCategoryId());

            // 存在しないとき
            if (is_null($beforeAttributeCategory)) {
                throw new NotFoundException("Not found attribute category.");
            }

            // 更新
            $this->attributeCategoryRepositoryImpl->updateAttributeCategory($attributeCategory);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }
    }
}
