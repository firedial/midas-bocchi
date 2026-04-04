<?php

namespace App\Infrastructure\Repository\Impl;

use App\Domain\Entities\TemplateDetailEntity;
use App\Domain\Entities\TemplateEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\TemplateId;
use App\Infrastructure\Repository\Concerns\HandlesQueryException;
use App\Infrastructure\Repository\TemplateRepositoryInterface;
use App\Models\DataModels\TemplateDataModel;
use App\Models\DataModels\TemplateDetailDataModel;
use Illuminate\Database\QueryException;

class TemplateRepositoryImpl implements TemplateRepositoryInterface
{
    use HandlesQueryException;
    public function getTemplates(): array
    {
        $rows = TemplateDataModel::selectTemplate();

        return array_map(
            fn ($row) => new TemplateEntity(
                TemplateId::filledId($row->id),
                $row->name,
            ),
            $rows
        );
    }

    public function selectTemplate(TemplateId $templateId): ?TemplateEntity
    {
        $rows = TemplateDataModel::selectTemplate($templateId->value());
        if (count($rows) === 0) {
            return null;
        }

        $details = $this->mapDetails(
            TemplateDetailDataModel::selectTemplateDetails($templateId->value())
        );

        return new TemplateEntity(
            TemplateId::filledId($rows[0]->id),
            $rows[0]->name,
            $details,
        );
    }

    public function insertTemplate(TemplateEntity $template): TemplateEntity
    {
        $row = TemplateDataModel::insertTemplate($template->name());

        try {
            TemplateDetailDataModel::insertTemplateDetails(
                $this->buildDetailRows($row->id, $template->details())
            );
        } catch (QueryException $e) {
            self::handleQueryException($e, "Insert template detail error.");
        }

        return new TemplateEntity(
            TemplateId::filledId($row->id),
            $row->name,
            $template->details(),
        );
    }

    public function updateTemplate(TemplateEntity $template): TemplateEntity
    {
        $row = TemplateDataModel::updateTemplate(
            $template->templateId()->value(),
            $template->name(),
        );

        TemplateDetailDataModel::deleteTemplateDetails($template->templateId()->value());

        try {
            TemplateDetailDataModel::insertTemplateDetails(
                $this->buildDetailRows($row->id, $template->details())
            );
        } catch (QueryException $e) {
            self::handleQueryException($e, "Update template detail error.");
        }

        return new TemplateEntity(
            TemplateId::filledId($row->id),
            $row->name,
            $template->details(),
        );
    }

    public function deleteTemplate(TemplateId $templateId): TemplateEntity
    {
        $row = TemplateDataModel::deleteTemplate($templateId->value());

        return new TemplateEntity(
            TemplateId::filledId($row->id),
            $row->name,
        );
    }

    /** @return TemplateDetailEntity[] */
    private function mapDetails(array $rows): array
    {
        return array_map(
            fn ($row) => new TemplateDetailEntity(
                seq: $row->seq,
                type: $row->type,
                amount: new Amount($row->amount),
                item: new Item($row->item),
                kindElementId: $row->kind_element_id,
                purposeElementId: $row->purpose_element_id,
                placeElementId: $row->place_element_id,
                moveBeforePurposeId: $row->move_before_purpose_id,
                moveAfterPurposeId: $row->move_after_purpose_id,
                moveBeforePlaceId: $row->move_before_place_id,
                moveAfterPlaceId: $row->move_after_place_id,
            ),
            $rows
        );
    }

    /** @param TemplateDetailEntity[] $details */
    private function buildDetailRows(int $templateId, array $details): array
    {
        return array_map(
            fn (TemplateDetailEntity $d) => [
                'template_id' => $templateId,
                'seq' => $d->seq(),
                'type' => $d->type(),
                'amount' => $d->amount()->value(),
                'item' => $d->item()->value(),
                'kind_element_id' => $d->kindElementId(),
                'purpose_element_id' => $d->purposeElementId(),
                'place_element_id' => $d->placeElementId(),
                'move_before_purpose_id' => $d->moveBeforePurposeId(),
                'move_after_purpose_id' => $d->moveAfterPurposeId(),
                'move_before_place_id' => $d->moveBeforePlaceId(),
                'move_after_place_id' => $d->moveAfterPlaceId(),
            ],
            $details
        );
    }
}
