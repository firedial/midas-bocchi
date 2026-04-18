<?php

namespace App\Usecases\Template;

use App\Domain\Entities\TemplateEntity;
use App\Infrastructure\Repository\TemplateRepositoryInterface;
use App\Infrastructure\Repository\Impl\TemplateRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class InsertTemplateUsecase
{
    private TemplateRepositoryImpl $templateRepository;

    public function __construct(?TemplateRepositoryInterface $templateRepository = null)
    {
        $this->templateRepository = $templateRepository ?: new TemplateRepositoryImpl();
    }

    public function execute(TemplateEntity $template): TemplateEntity
    {
        DB::beginTransaction();
        try {
            $result = $this->templateRepository->insertTemplate($template);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $result;
    }
}
