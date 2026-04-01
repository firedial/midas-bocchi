<?php

namespace App\Usecases\Scenario;

use App\Infrastructure\Repository\ScenarioRepositoryInterface;
use App\Infrastructure\Repository\Impl\ScenarioRepositoryImpl;
use Exception;
use Illuminate\Support\Facades\DB;

class GetScenariosUsecase
{
    private ScenarioRepositoryImpl $scenarioRepository;

    public function __construct(?ScenarioRepositoryInterface $scenarioRepository = null)
    {
        $this->scenarioRepository = $scenarioRepository ?: new ScenarioRepositoryImpl();
    }

    public function execute(): array
    {
        DB::beginTransaction();
        try {
            $scenarios = $this->scenarioRepository->getScenarios();
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $scenarios;
    }
}
