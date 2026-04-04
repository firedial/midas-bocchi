<?php

namespace App\Infrastructure\Repository;

use App\Domain\Entities\TemplateEntity;
use App\Domain\ValueObjects\TemplateId;

interface TemplateRepositoryInterface
{
    /** @return TemplateEntity[] 明細なし */
    public function getTemplates(): array;

    /** 明細付きで取得。存在しない場合は null */
    public function selectTemplate(TemplateId $templateId): ?TemplateEntity;

    public function insertTemplate(TemplateEntity $template): TemplateEntity;

    public function updateTemplate(TemplateEntity $template): TemplateEntity;

    public function deleteTemplate(TemplateId $templateId): TemplateEntity;
}
