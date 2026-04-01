<?php

namespace App\Usecases\Scenario;

use App\Domain\Entities\ScenarioEntity;
use App\Infrastructure\Repository\ScenarioRepositoryInterface;
use App\Infrastructure\Repository\Impl\ScenarioRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class InsertScenarioUsecase
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
            $result = $this->scenarioRepository->insertScenario($scenario);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $result;
    }
}
