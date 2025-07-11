<?php

namespace App\Domain\Entities;

use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\AttributeCategoryId;
use App\Domain\ValueObjects\AttributeElementId;
use App\Domain\ValueObjects\AttributeElementName;
use App\Domain\ValueObjects\Description;
use App\Domain\ValueObjects\Priority;

class AttributeElementEntity
{
    public function __construct(
        protected readonly Attribute $attribute,
        protected readonly AttributeElementId $attributeElementId,
        protected readonly AttributeElementName $attributeElementName,
        protected readonly Description $description,
        protected readonly Priority $priority,
        protected readonly AttributeCategoryId $attributeCategoryId,
    ) {}

    public function attribute(): Attribute
    {
        return $this->attribute;
    }

    public function attributeElementId(): AttributeElementId
    {
        return $this->attributeElementId;
    }

    public function attributeElementName(): AttributeElementName
    {
        return $this->attributeElementName;
    }

    public function description(): Description
    {
        return $this->description;
    }

    public function priority(): Priority
    {
        return $this->priority;
    }

    public function attributeCategoryId(): AttributeCategoryId
    {
        return $this->attributeCategoryId;
    }
}
