<?php

namespace App\Infrastructure\Repository\Impl;

use App\Domain\Entities\TemplateDetailEntity;
use App\Domain\Entities\TemplateEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\TemplateDetailType;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Domain\ValueObjects\TemplateId;
use App\Domain\ValueObjects\TemplateName;
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
                new TemplateName($row->name),
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
            new TemplateName($rows[0]->name),
            $details,
        );
    }

    public function insertTemplate(TemplateEntity $template): TemplateEntity
    {
        $row = TemplateDataModel::insertTemplate($template->name()->value());

        try {
            TemplateDetailDataModel::insertTemplateDetails(
                $this->buildDetailRows($row->id, $template->details())
            );
        } catch (QueryException $e) {
            self::handleQueryException($e, "Insert template detail error.");
        }

        return new TemplateEntity(
            TemplateId::filledId($row->id),
            new TemplateName($row->name),
            $template->details(),
        );
    }

    public function updateTemplate(TemplateEntity $template): TemplateEntity
    {
        $row = TemplateDataModel::updateTemplate(
            $template->templateId()->value(),
            $template->name()->value(),
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
            new TemplateName($row->name),
            $template->details(),
        );
    }

    public function deleteTemplate(TemplateId $templateId): TemplateEntity
    {
        $row = TemplateDataModel::deleteTemplate($templateId->value());

        return new TemplateEntity(
            TemplateId::filledId($row->id),
            new TemplateName($row->name),
        );
    }

    /** @return TemplateDetailEntity[] */
    private function mapDetails(array $rows): array
    {
        return array_map(
            fn ($row) => new TemplateDetailEntity(
                seq: $row->seq,
                type: new TemplateDetailType($row->type),
                amount: new Amount($row->amount),
                item: new Item($row->item),
                kindElementId: KindElementId::filledId($row->kind_element_id),
                purposeElementId: is_null($row->purpose_element_id) ? null : PurposeElementId::filledId($row->purpose_element_id),
                placeElementId: is_null($row->place_element_id) ? null : PlaceElementId::filledId($row->place_element_id),
                moveBeforePurposeId: is_null($row->move_before_purpose_id) ? null : PurposeElementId::filledId($row->move_before_purpose_id),
                moveAfterPurposeId: is_null($row->move_after_purpose_id) ? null : PurposeElementId::filledId($row->move_after_purpose_id),
                moveBeforePlaceId: is_null($row->move_before_place_id) ? null : PlaceElementId::filledId($row->move_before_place_id),
                moveAfterPlaceId: is_null($row->move_after_place_id) ? null : PlaceElementId::filledId($row->move_after_place_id),
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
                'type' => $d->type()->value(),
                'amount' => $d->amount()->value(),
                'item' => $d->item()->value(),
                'kind_element_id' => $d->kindElementId()->value(),
                'purpose_element_id' => $d->purposeElementId()?->value(),
                'place_element_id' => $d->placeElementId()?->value(),
                'move_before_purpose_id' => $d->moveBeforePurposeId()?->value(),
                'move_after_purpose_id' => $d->moveAfterPurposeId()?->value(),
                'move_before_place_id' => $d->moveBeforePlaceId()?->value(),
                'move_after_place_id' => $d->moveAfterPlaceId()?->value(),
            ],
            $details
        );
    }
}
