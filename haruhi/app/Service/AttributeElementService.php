<?php

namespace App\Service;

use App\Models\KindElement;
use App\Models\PurposeElement;
use App\Models\PlaceElement;
use App\Models\Balance;
use App\Exceptions\InvalidParameterException;
use App\Models\Dao\AttributeElementDao;
use App\Models\Dao\Impl\AttributeElementDaoImpl;
use Illuminate\Support\Facades\DB;

/**
 *  attributeEelement テーブルの操作のサービスクラス
 */
class AttributeElementService
{
    private $attributeElementDao;

    public function __construct(AttributeElementDao $attributeElementDao = null)
    {
        $this->attributeElementDao = $attributeElementDao ?: new AttributeElementDaoImpl();
    }

    public function getAttributeElements(array $input): array
    {
        $records = $this->attributeElementDao->getAttributeElement($input['attributeName']);
        return array_map(
            function ($record) {
                return [
                    'id' => $record->id,
                    'name' => $record->name,
                    'description' => $record->description,
                    'category_id' => $record->category_id,
                ];
            },
            $records
        );
    }

    public function getAttributeElementByElementId(array $input): array
    {
        $records = $this->attributeElementDao->getAttributeElementByElementId($input['attributeName'], $input['elementId']);
        return array_map(
            function ($record) {
                return [
                    'id' => $record->id,
                    'name' => $record->name,
                    'description' => $record->description,
                    'category_id' => $record->category_id,
                ];
            },
            $records
        );
    }

    public function createAttributeElement(array $input): Bool
    {
        return $this->attributeElementDao->insertAttributeElement($input['attributeName'], $input['attributeElement']);
    }

    public function updateAttributeElement(array $input): Bool
    {
        return $this->attributeElementDao->updateAttributeElement($input['attributeName'], $input['attributeElement']);
    }
}
