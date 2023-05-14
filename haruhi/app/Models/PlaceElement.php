<?php

namespace App\Models;

use Illuminate\Database\Eloquent\Factories\HasFactory;
use Illuminate\Database\Eloquent\Model;

class PlaceElement extends AttributeElement
{
    const MOVE_ID = 1;

    protected $table = 'm_place_element';
}
