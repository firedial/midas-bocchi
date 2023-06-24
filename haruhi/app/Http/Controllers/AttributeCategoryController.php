<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use App\Models\Balance;
use App\Models\KindElement;
use App\Service\AttributeCategoryService;
use App\Exceptions\InvalidParameterException;

class AttributeCategoryController extends Controller
{
    public function index(String $attributeName)
    {
        $attributeCategoryService = new AttributeCategoryService();

        if (!in_array($attributeName, ['kind_category', 'purpose_category', 'place_category'])) {
            throw new InvalidParameterException("Wrong attribute name {$attributeName}.");
        }

        return $attributeCategoryService->getAttributeCategories(['attributeName' => $attributeName]);
    }

    public function store(Request $request, String $attributeName)
    {
        $attributeCategoryService = new AttributeCategoryService();

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

        return $attributeCategoryService->createAttributeCategory([
            'attributeName' => $attributeName,
            'attributeCategory' => [
                'name' => $name,
                'description' => $request->input('description'),
            ],
        ]);
    }

    public function update(Request $request, String $attributeName, int $categoryId)
    {
        $attributeCategoryService = new AttributeCategoryService();

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

        return $attributeCategoryService->updateAttributeCategory([
            'attributeName' => $attributeName,
            'attributeCategory' => [
                'id' => $categoryId,
                'name' => $request->input('name'),
                'description' => $request->input('description'),
            ],
        ]);
    }
}
