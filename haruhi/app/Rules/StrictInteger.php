<?php

namespace App\Rules;

use Closure;
use Illuminate\Contracts\Validation\ValidationRule;

class StrictInteger implements ValidationRule
{
    public bool $implicit = true;  // ← これを追加

    public function validate(string $attribute, mixed $value, Closure $fail): void
    {
        if (!is_int($value)) {
            $fail(':attribute must be an integer type.');
        }
    }
}
