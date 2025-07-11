<?php

namespace App\Domain\Entities;

use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\AttributeCategoryId;
use App\Domain\ValueObjects\AttributeCategoryName;
use App\Domain\ValueObjects\Description;

class AttributeCategoryEntity
{
    public function __construct(
        protected readonly Attribute $attribute,
        protected readonly AttributeCategoryId $attributeCategoryId,
        protected readonly AttributeCategoryName $attributeCategoryName,
        protected readonly Description $description,
    ) {}

    public function attribute(): Attribute
    {
        return $this->attribute;
    }

    public function attributeCategoryId(): AttributeCategoryId
    {
        return $this->attributeCategoryId;
    }

    public function attributeCategoryName(): AttributeCategoryName
    {
        return $this->attributeCategoryName;
    }

    public function description(): Description
    {
        return $this->description;
    }
}
