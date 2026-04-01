<?php

namespace App\Usecases\Scenario;

use App\Domain\Entities\ScenarioEntity;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;
use App\Infrastructure\Repository\ScenarioRepositoryInterface;
use App\Infrastructure\Repository\Impl\ScenarioRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class UpdateScenarioUsecase
{
    private ScenarioRepositoryImpl $scenarioRepository;

    public function __construct(?ScenarioRepositoryInterface $scenarioRepository = null)
    {
        $this->scenarioRepository = $scenarioRepository ?: new ScenarioRepositoryImpl();
    }

    public function execute(ScenarioEntity $scenario): ScenarioEntity
    {
        DB::beginTransaction();
        try {
            $before = $this->scenarioRepository->selectScenario($scenario->scenarioId());

            if (is_null($before)) {
                throw new AppException(ErrorCode::RECORD_NOT_FOUND, 'Not found scenario.');
            }

            $result = $this->scenarioRepository->updateScenario($scenario);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $result;
    }
}
