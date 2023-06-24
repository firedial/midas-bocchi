<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use App\Models\Balance;
use App\Models\KindElement;
use App\Service\AttributeCategoryService;

class AttributeCategoryController extends Controller
{
    public function index(String $attributeName)
    {
        $attributeCategoryService = new AttributeCategoryService();
        return $attributeCategoryService->getAttributeCategories(['attributeName' => $attributeName]);
    }

    public function store(Request $request, String $attributeName)
    {
        $attributeCategoryService = new AttributeCategoryService();
        return $attributeCategoryService->createAttributeCategory([
            'attributeName' => $attributeName,
            'attributeCategory' => [
                'name' => $request->input('name'),
                'description' => $request->input('description'),
            ],
        ]);
    }

    public function update(Request $request, String $attributeName, int $categoryId)
    {
        $attributeCategoryService = new AttributeCategoryService();
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
