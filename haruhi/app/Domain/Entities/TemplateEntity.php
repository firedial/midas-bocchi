<?php

namespace App\Domain\Entities;

use App\Domain\ValueObjects\TemplateId;

class TemplateEntity
{
    public function __construct(
        protected readonly TemplateId $templateId,
        protected readonly string $name,
        protected readonly array $details = [],
    ) {}

    public function templateId(): TemplateId
    {
        return $this->templateId;
    }

    public function name(): string
    {
        return $this->name;
    }

    /** @return TemplateDetailEntity[] */
    public function details(): array
    {
        return $this->details;
    }
}
