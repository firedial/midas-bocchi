<?php

namespace App\Models;

use Illuminate\Database\Eloquent\Factories\HasFactory;
use Illuminate\Database\Eloquent\Model;

class KindElement extends AttributeElement
{
    const MOVE_ID = 1;

    protected $table = 'm_kind_element';
}
