<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use App\Service\AttributeCategoryService;
use App\Exceptions\InvalidParameterException;

class AttributeCategoryController extends Controller
{
    public function index(string $attributeName)
    {
        if (!in_array($attributeName, ['kind_category', 'purpose_category', 'place_category'])) {
            throw new InvalidParameterException("Wrong attribute name {$attributeName}.");
        }

        $attributeCategoryService = new AttributeCategoryService();
        return $attributeCategoryService->getAttributeCategories(['attributeName' => $attributeName]);
    }

    public function store(Request $request, string $attributeName)
    {
        if (!in_array($attributeName, ['kind_category', 'purpose_category', 'place_category'])) {
            throw new InvalidParameterException("Wrong attribute name {$attributeName}.");
        }

        $name = $request->input('name');
        if (!preg_match('/^[A-Z]\w*$/', $name) || strlen($name) > 20) {
            throw new InvalidParameterException("Wrong name {$name}.");
        }

        $description = $request->input('description');
        if (empty($description) || mb_strlen($description) > 20) {
            throw new InvalidParameterException("Wrong description {$description}.");
        }

        $attributeCategoryService = new AttributeCategoryService();
        return $attributeCategoryService->createAttributeCategory([
            'attributeName' => $attributeName,
            'attributeCategory' => [
                'name' => $name,
                'description' => $description,
            ],
        ]);
    }

    public function update(Request $request, string $attributeName, int $categoryId)
    {
        if (!in_array($attributeName, ['kind_category', 'purpose_category', 'place_category'])) {
            throw new InvalidParameterException("Wrong attribute name {$attributeName}.");
        }

        if (empty($categoryId)) {
            throw new InvalidParameterException("categoryId is empty.");
        }

        $name = $request->input('name');
        if (!preg_match('/^[A-Z]\w*$/', $name) || strlen($name) > 20) {
            throw new InvalidParameterException("Wrong name {$name}.");
        }

        $description = $request->input('description');
        if (empty($description) || mb_strlen($description) > 20) {
            throw new InvalidParameterException("Wrong description {$description}.");
        }

        $attributeCategoryService = new AttributeCategoryService();
        return $attributeCategoryService->updateAttributeCategory([
            'attributeName' => $attributeName,
            'attributeCategory' => [
                'id' => $categoryId,
                'name' => $name,
                'description' => $description,
            ],
        ]);
    }
}
