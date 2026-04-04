<?php

namespace App\Usecases\Template;

use App\Infrastructure\Repository\TemplateRepositoryInterface;
use App\Infrastructure\Repository\Impl\TemplateRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class GetTemplatesUsecase
{
    private TemplateRepositoryImpl $templateRepository;

    public function __construct(?TemplateRepositoryInterface $templateRepository = null)
    {
        $this->templateRepository = $templateRepository ?: new TemplateRepositoryImpl();
    }

    public function execute(): array
    {
        DB::beginTransaction();
        try {
            $templates = $this->templateRepository->getTemplates();
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $templates;
    }
}
