<?php

namespace App\Usecases\AttributeElement;

use App\Domain\Entities\AttributeElementEntity;
use App\Exceptions\NotFoundException;
use App\Infrastructure\Repository\AttributeElementRepositoryInterface;
use App\Infrastructure\Repository\Impl\AttributeElementRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class UpdateAttributeElementUsecase
{
    private AttributeElementRepositoryImpl $attributeElementRepositoryImpl;

    public function __construct(?AttributeElementRepositoryInterface $attributeEelemntRepository = null)
    {
        $this->attributeElementRepositoryImpl = $attributeEelemntRepository ?: new AttributeElementRepositoryImpl();
    }

    public function execute(AttributeElementEntity $attributeElement): void
    {
        DB::beginTransaction();
        try {
            $beforeAttributeElement = $this->attributeElementRepositoryImpl->selectAttributeElement($attributeElement->attribute(), $attributeElement->attributeElementId());

            // 存在しないとき
            if (is_null($beforeAttributeElement)) {
                throw new NotFoundException("Not found attribute element.");
            }

            // 更新
            $this->attributeElementRepositoryImpl->updateAttributeElement($attributeElement);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }
    }
}
