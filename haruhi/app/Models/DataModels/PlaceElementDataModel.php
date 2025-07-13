<?php

namespace App\Models\DataModels;

class PlaceElementDataModel extends AttributeElementDataModel
{
    public const TABLE_NAME = "m_place_element";

    public const C_ID = "id";
    public const C_NAME = "name";
    public const C_DESCRIPTION = "description";
    public const C_PRIORITY = "priority";
    public const C_CATEGORY_ID = "category_id";
}
