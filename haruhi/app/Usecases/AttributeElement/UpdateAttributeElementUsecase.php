<?php

namespace App\Usecases\AttributeElement;

use App\Domain\Entities\AttributeElementEntity;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;
use App\Infrastructure\Repository\AttributeElementRepositoryInterface;
use App\Infrastructure\Repository\Impl\AttributeElementRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class UpdateAttributeElementUsecase
{
    private AttributeElementRepositoryImpl $attributeElementRepositoryImpl;

    public function __construct(?AttributeElementRepositoryInterface $attributeElemntRepository = null)
    {
        $this->attributeElementRepositoryImpl = $attributeElemntRepository ?: new AttributeElementRepositoryImpl();
    }

    public function execute(AttributeElementEntity $attributeElement): AttributeElementEntity
    {
        DB::beginTransaction();
        try {
            $beforeAttributeElement = $this->attributeElementRepositoryImpl->selectAttributeElement($attributeElement->attribute(), $attributeElement->attributeElementId());

            // 存在しないとき
            if (is_null($beforeAttributeElement)) {
                throw new AppException(ErrorCode::RECORD_NOT_FOUND, "Not found attribute element.");
            }

            // 更新
            $result = $this->attributeElementRepositoryImpl->updateAttributeElement($attributeElement);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $result;
    }
}
