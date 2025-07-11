<?php

namespace App\Usecases\AttributeCategory;

use App\Domain\Entities\AttributeCategoryEntity;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\AttributeCategoryId;
use App\Exceptions\NotFoundException;
use App\Infrastructure\Repository\AttributeCategoryRepositoryInterface;
use App\Infrastructure\Repository\Impl\AttributeCategoryRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class SelectAttributeCategoryUsecase
{
    private AttributeCategoryRepositoryImpl $attributeCategoryRepositoryImpl;

    public function __construct(?AttributeCategoryRepositoryInterface $attributeCategoryRepository = null)
    {
        $this->attributeCategoryRepositoryImpl = $attributeCategoryRepository ?: new AttributeCategoryRepositoryImpl();
    }

    public function execute(Attribute $attribute, AttributeCategoryId $attributeCategoryId): AttributeCategoryEntity
    {
        DB::beginTransaction();
        try {
            // 取得
            $attributeCategory = $this->attributeCategoryRepositoryImpl->selectAttributeCategory($attribute, $attributeCategoryId);

            // 存在しないとき
            if (is_null($attributeCategory)) {
                throw new NotFoundException("Not found attribute category.");
            }
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $attributeCategory;
    }
}
