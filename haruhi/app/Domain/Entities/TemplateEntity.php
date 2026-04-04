<?php

namespace App\Domain\Entities;

use App\Domain\ValueObjects\TemplateId;
use App\Domain\ValueObjects\TemplateName;

class TemplateEntity
{
    public function __construct(
        protected readonly TemplateId $templateId,
        protected readonly TemplateName $name,
        protected readonly array $details = [],
    ) {}

    public function templateId(): TemplateId
    {
        return $this->templateId;
    }

    public function name(): TemplateName
    {
        return $this->name;
    }

    /** @return TemplateDetailEntity[] */
    public function details(): array
    {
        return $this->details;
    }
}
