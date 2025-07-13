<?php

namespace App\Usecases\AttributeElement;

use App\Domain\Entities\AttributeElementEntity;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\AttributeElementId;
use App\Exceptions\NotFoundException;
use App\Infrastructure\Repository\AttributeElementRepositoryInterface;
use App\Infrastructure\Repository\Impl\AttributeElementRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class SelectAttributeElementUsecase
{
    private AttributeElementRepositoryImpl $attributeElementRepositoryImpl;

    public function __construct(?AttributeElementRepositoryInterface $attributeElemntRepository = null)
    {
        $this->attributeElementRepositoryImpl = $attributeElemntRepository ?: new AttributeElementRepositoryImpl();
    }

    public function execute(Attribute $attribute, AttributeElementId $attributeElementId): AttributeElementEntity
    {
        DB::beginTransaction();
        try {
            // 取得
            $attributeElement = $this->attributeElementRepositoryImpl->selectAttributeElement($attribute, $attributeElementId);

            // 存在しないとき
            if (is_null($attributeElement)) {
                throw new NotFoundException("Not found attribute element.");
            }
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $attributeElement;
    }
}
