<?php

namespace App\Service;

use App\Models\KindElement;
use App\Models\PurposeElement;
use App\Models\PlaceElement;
use App\Models\Balance;
use App\Exceptions\InvalidParameterException;
use App\Models\Dao\AttributeCategoryDao;
use App\Models\Dao\Impl\AttributeCategoryDaoImpl;
use Illuminate\Support\Facades\DB;

/**
 *  attributeCategory テーブルの操作のサービスクラス
 */
class AttributeCategoryService
{
    private $attributeCategoryDao;

    public function __construct(AttributeCategoryDao $attributeCategoryDao = null)
    {
        $this->attributeCategoryDao = $attributeCategoryDao ?: new AttributeCategoryDaoImpl();
    }

    public function getAttributeCategories(array $input): array
    {
        $records = $this->attributeCategoryDao->getAttributeCategories($input['attributeName']);
        return array_map(
            function ($record) {
                return [
                    'id' => $record->id,
                    'name' => $record->name,
                    'description' => $record->description,
                ];
            },
            $records
        );
    }

    public function createAttributeCategory(array $input): bool
    {
        return $this->attributeCategoryDao->insertAttributeCategory($input['attributeName'], $input['attributeCategory']);
    }

    public function updateAttributeCategory(array $input): bool
    {
        return $this->attributeCategoryDao->updateAttributeCategory($input['attributeName'], $input['attributeCategory']);
    }
}
