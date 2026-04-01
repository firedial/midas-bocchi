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

class SelectScenarioUsecase
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
            $scenario = $this->scenarioRepository->selectScenario($scenarioId);

            if (is_null($scenario)) {
                throw new AppException(ErrorCode::RECORD_NOT_FOUND, 'Not found scenario.');
            }

            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $scenario;
    }
}
