<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use App\Service\AttributeElementService;
use App\Exceptions\InvalidParameterException;

class AttributeElementController extends Controller
{
    public function index(String $attributeName)
    {
        if (!in_array($attributeName, ['kind_element', 'purpose_element', 'place_element'])) {
            throw new InvalidParameterException("Wrong attribute name {$attributeName}.");
        }

        $attributeElementService = new AttributeElementService();
        return $attributeElementService->getAttributeElements(['attributeName' => $attributeName]);
    }

    public function show(String $attributeName, string $elementId)
    {
        if (!in_array($attributeName, ['kind_element', 'purpose_element', 'place_element'])) {
            throw new InvalidParameterException("Wrong attribute name {$attributeName}.");
        }

        if (empty($elementId)) {
            throw new InvalidParameterException("Element is empty.");
        }

        $attributeElementService = new AttributeElementService();
        return $attributeElementService->getAttributeElementByElementId(['attributeName' => $attributeName, 'elementId' => $elementId]);
    }

    public function store(Request $request, String $attributeName)
    {
        if (!in_array($attributeName, ['kind_element', 'purpose_element', 'place_element'])) {
            throw new InvalidParameterException("Wrong attribute name {$attributeName}.");
        }

        $name = $request->input('name');
        if (!preg_match('/^[a-z]\w*$/', $name) || strlen($name) > 20) {
            throw new InvalidParameterException("Wrong name {$name}.");
        }

        $description = $request->input('description');
        if (empty($description) || mb_strlen($description) > 20) {
            throw new InvalidParameterException("Wrong description {$description}.");
        }

        $categoryId = $request->input('category_id');
        if (empty($categoryId)) {
            throw new InvalidParameterException("Wrong category id.");
        }

        // @todo 移動カテゴリが選べないようにする

        $attributeElementService = new AttributeElementService();
        return $attributeElementService->createAttributeElement([
            'attributeName' => $attributeName,
            'attributeElement' => [
                'name' => $name,
                'description' => $description,
                'categoryId' => $categoryId,
            ],
        ]);
    }

    public function update(Request $request, String $attributeName, int $elementId)
    {
        if (!in_array($attributeName, ['kind_element', 'purpose_element', 'place_element'])) {
            throw new InvalidParameterException("Wrong attribute name {$attributeName}.");
        }

        if (empty($elementId)) {
            throw new InvalidParameterException("Element is empty.");
        }

        $name = $request->input('name');
        if (!preg_match('/^[a-z]\w*$/', $name) || strlen($name) > 20) {
            throw new InvalidParameterException("Wrong name {$name}.");
        }

        $description = $request->input('description');
        if (empty($description) || mb_strlen($description) > 20) {
            throw new InvalidParameterException("Wrong description {$description}.");
        }

        $categoryId = $request->input('category_id');
        if (empty($categoryId)) {
            throw new InvalidParameterException("Wrong category id.");
        }

        // @todo 移動カテゴリが選べないようにする

        $attributeElementService = new AttributeElementService();
        return $attributeElementService->updateAttributeElement([
            'attributeName' => $attributeName,
            'attributeElement' => [
                'id' => $elementId,
                'name' => $name,
                'description' => $description,
                'categoryId' => $categoryId,
            ],
        ]);
    }
}
