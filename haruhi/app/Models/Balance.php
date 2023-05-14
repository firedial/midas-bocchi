<?php

namespace App\Models;

use Illuminate\Database\Eloquent\Factories\HasFactory;
use Illuminate\Database\Eloquent\Model;

class Balance extends Model
{
    use HasFactory;

    protected $table = 'm_balance';

    protected $fillable = [
        'amount',
        'item',
        'kind_element_id',
        'purpose_element_id',
        'place_element_id',
        'date',
    ];
}
