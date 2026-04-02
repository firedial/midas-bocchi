<?php

namespace App\Infrastructure\Repository\Impl;

use App\Domain\Entities\ScenarioDetailEntity;
use App\Domain\Entities\ScenarioEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\ScenarioId;
use App\Infrastructure\Repository\ScenarioRepositoryInterface;
use App\Models\DataModels\ScenarioDataModel;
use App\Models\DataModels\ScenarioDetailDataModel;

class ScenarioRepositoryImpl implements ScenarioRepositoryInterface
{
    public function getScenarios(): array
    {
        $rows = ScenarioDataModel::selectScenario();

        return array_map(
            fn ($row) => new ScenarioEntity(
                ScenarioId::filledId($row->id),
                $row->name,
            ),
            $rows
        );
    }

    public function selectScenario(ScenarioId $scenarioId): ?ScenarioEntity
    {
        $rows = ScenarioDataModel::selectScenario($scenarioId->value());
        if (count($rows) === 0) {
            return null;
        }

        $details = $this->mapDetails(
            ScenarioDetailDataModel::selectScenarioDetails($scenarioId->value())
        );

        return new ScenarioEntity(
            ScenarioId::filledId($rows[0]->id),
            $rows[0]->name,
            $details,
        );
    }

    public function insertScenario(ScenarioEntity $scenario): ScenarioEntity
    {
        $row = ScenarioDataModel::insertScenario($scenario->name());

        ScenarioDetailDataModel::insertScenarioDetails(
            $this->buildDetailRows($row->id, $scenario->details())
        );

        return new ScenarioEntity(
            ScenarioId::filledId($row->id),
            $row->name,
            $scenario->details(),
        );
    }

    public function updateScenario(ScenarioEntity $scenario): ScenarioEntity
    {
        $row = ScenarioDataModel::updateScenario(
            $scenario->scenarioId()->value(),
            $scenario->name(),
        );

        ScenarioDetailDataModel::deleteScenarioDetails($scenario->scenarioId()->value());
        ScenarioDetailDataModel::insertScenarioDetails(
            $this->buildDetailRows($row->id, $scenario->details())
        );

        return new ScenarioEntity(
            ScenarioId::filledId($row->id),
            $row->name,
            $scenario->details(),
        );
    }

    public function deleteScenario(ScenarioId $scenarioId): ScenarioEntity
    {
        $row = ScenarioDataModel::deleteScenario($scenarioId->value());

        return new ScenarioEntity(
            ScenarioId::filledId($row->id),
            $row->name,
        );
    }

    /** @return ScenarioDetailEntity[] */
    private function mapDetails(array $rows): array
    {
        return array_map(
            fn ($row) => new ScenarioDetailEntity(
                seq: $row->seq,
                type: $row->type,
                amount: new Amount($row->amount),
                item: new Item($row->item),
                typeElementId: $row->type_element_id,
                purposeElementId: $row->purpose_element_id,
                placeElementId: $row->place_element_id,
                moveAttribute: $row->move_attribute,
                moveBeforeId: $row->move_before_id,
                moveAfterId: $row->move_after_id,
            ),
            $rows
        );
    }

    /** @param ScenarioDetailEntity[] $details */
    private function buildDetailRows(int $scenarioId, array $details): array
    {
        return array_map(
            fn (ScenarioDetailEntity $d) => [
                'scenario_id' => $scenarioId,
                'seq' => $d->seq(),
                'type' => $d->type(),
                'amount' => $d->amount()->value(),
                'item' => $d->item()->value(),
                'type_element_id' => $d->typeElementId(),
                'purpose_element_id' => $d->purposeElementId(),
                'place_element_id' => $d->placeElementId(),
                'move_attribute' => $d->moveAttribute(),
                'move_before_id' => $d->moveBeforeId(),
                'move_after_id' => $d->moveAfterId(),
            ],
            $details
        );
    }
}
