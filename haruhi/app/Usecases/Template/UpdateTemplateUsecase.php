<?php

namespace App\Usecases\Template;

use App\Domain\Entities\TemplateEntity;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;
use App\Infrastructure\Repository\TemplateRepositoryInterface;
use App\Infrastructure\Repository\Impl\TemplateRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class UpdateTemplateUsecase
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
            $before = $this->templateRepository->selectTemplate($template->templateId());

            if (is_null($before)) {
                throw new AppException(ErrorCode::RECORD_NOT_FOUND, 'Not found template.');
            }

            $result = $this->templateRepository->updateTemplate($template);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $result;
    }
}
