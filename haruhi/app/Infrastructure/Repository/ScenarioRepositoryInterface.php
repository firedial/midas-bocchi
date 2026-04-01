<?php

namespace App\Infrastructure\Repository;

use App\Domain\Entities\ScenarioEntity;
use App\Domain\ValueObjects\ScenarioId;

interface ScenarioRepositoryInterface
{
    /** @return ScenarioEntity[] 明細なし */
    public function getScenarios(): array;

    /** 明細付きで取得。存在しない場合は null */
    public function selectScenario(ScenarioId $scenarioId): ?ScenarioEntity;

    public function insertScenario(ScenarioEntity $scenario): ScenarioEntity;

    public function updateScenario(ScenarioEntity $scenario): ScenarioEntity;

    public function deleteScenario(ScenarioId $scenarioId): ScenarioEntity;
}
