<?php

namespace App\Infrastructure\Repository\Impl;

use App\Domain\Entities\TemplateDetailEntity;
use App\Domain\Entities\TemplateEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\TemplateId;
use App\Infrastructure\Repository\TemplateRepositoryInterface;
use App\Models\DataModels\TemplateDataModel;
use App\Models\DataModels\TemplateDetailDataModel;

class TemplateRepositoryImpl implements TemplateRepositoryInterface
{
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

        TemplateDetailDataModel::insertTemplateDetails(
            $this->buildDetailRows($row->id, $template->details())
        );

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
        TemplateDetailDataModel::insertTemplateDetails(
            $this->buildDetailRows($row->id, $template->details())
        );

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
