<?php

namespace App\Rules;

use Closure;
use Illuminate\Contracts\Validation\ValidationRule;

class StrictInteger implements ValidationRule
{
    public bool $implicit = true;

    public function __construct(private bool $nullable = false) {}

    public function validate(string $attribute, mixed $value, Closure $fail): void
    {
        if ($this->nullable && is_null($value)) {
            return;
        }

        if (!is_int($value)) {
            $fail(':attribute must be an integer type.');
        }
    }
}
