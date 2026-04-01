<?php

namespace App\Usecases\Scenario;

use App\Domain\Entities\ScenarioEntity;
use App\Domain\ValueObjects\ScenarioId;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;
use App\Infrastructure\Repository\ScenarioRepositoryInterface;
use App\Infrastructure\Repository\Impl\ScenarioRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class DeleteScenarioUsecase
{
    private ScenarioRepositoryImpl $scenarioRepository;

    public function __construct(?ScenarioRepositoryInterface $scenarioRepository = null)
    {
        $this->scenarioRepository = $scenarioRepository ?: new ScenarioRepositoryImpl();
    }

    public function execute(ScenarioId $scenarioId): ScenarioEntity
    {
        DB::beginTransaction();
        try {
            $before = $this->scenarioRepository->selectScenario($scenarioId);

            if (is_null($before)) {
                throw new AppException(ErrorCode::RECORD_NOT_FOUND, 'Not found scenario.');
            }

            $result = $this->scenarioRepository->deleteScenario($scenarioId);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $result;
    }
}
