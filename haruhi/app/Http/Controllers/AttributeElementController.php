<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use App\Models\Balance;
use App\Models\KindElement;
use App\Service\AttributeElementService;

class AttributeElementController extends Controller
{
    public function index(String $attributeName)
    {
        $attributeElementService = new AttributeElementService();
        return $attributeElementService->getAttributeElements(['attributeName' => $attributeName]);
    }

    public function show(String $attributeName, string $elementId)
    {
        $attributeElementService = new AttributeElementService();
        return $attributeElementService->getAttributeElementByElementId(['attributeName' => $attributeName, 'elementId' => $elementId]);
    }

    public function store(Request $request, String $attributeName)
    {
        $attributeElementService = new AttributeElementService();
        return $attributeElementService->createAttributeElement([
            'attributeName' => $attributeName,
            'attributeElement' => [
                'name' => $request->input('name'),
                'description' => $request->input('description'),
                'categoryId' => $request->input('category_id'),
            ],
        ]);
    }

    public function update(Request $request, String $attributeName, int $elementId)
    {
        $attributeElementService = new AttributeElementService();
        return $attributeElementService->updateAttributeElement([
            'attributeName' => $attributeName,
            'attributeElement' => [
                'id' => $elementId,
                'name' => $request->input('name'),
                'description' => $request->input('description'),
                'categoryId' => $request->input('category_id'),
            ],
        ]);
    }
}
