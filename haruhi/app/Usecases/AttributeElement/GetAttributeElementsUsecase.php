<?php

namespace App\Usecases\AttributeElement;

use App\Domain\ValueObjects\Attribute;
use App\Infrastructure\Repository\AttributeElementRepositoryInterface;
use App\Infrastructure\Repository\Impl\AttributeElementRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class GetAttributeElementsUsecase
{
    private AttributeElementRepositoryImpl $attributeElementRepositoryImpl;

    public function __construct(?AttributeElementRepositoryInterface $attributeEelemntRepository = null)
    {
        $this->attributeElementRepositoryImpl = $attributeEelemntRepository ?: new AttributeElementRepositoryImpl();
    }

    public function execute(Attribute $attribute): array
    {
        DB::beginTransaction();
        try {
            $attributeElements = $this->attributeElementRepositoryImpl->getAttributeElements($attribute);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $attributeElements;
    }
}
