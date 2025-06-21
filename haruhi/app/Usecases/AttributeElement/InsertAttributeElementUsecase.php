<?php

namespace App\Usecases\AttributeElement;

use App\Domain\Entities\AttributeElementEntity;
use App\Infrastructure\Repository\AttributeElementRepositoryInterface;
use App\Infrastructure\Repository\Impl\AttributeElementRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class InsertAttributeElementUsecase
{
    private AttributeElementRepositoryImpl $attributeElementRepositoryImpl;

    public function __construct(?AttributeElementRepositoryInterface $attributeEelemntRepository = null)
    {
        $this->attributeElementRepositoryImpl = $attributeEelemntRepository ?: new AttributeElementRepositoryImpl();
    }

    public function execute(AttributeElementEntity $attributeElement): int
    {
        DB::beginTransaction();
        try {
            // 挿入
            $insertId = $this->attributeElementRepositoryImpl->insertAttributeElement($attributeElement);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $insertId;
    }
}
