<?php

namespace App\Http\Controllers;

use App\Domain\Entities\FixedBalanceEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\FixedBalanceId;
use Illuminate\Http\Request;
use App\Exceptions\InvalidParameterException;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Usecases\FixedBalance\DeleteFixedBalanceUsecase;
use App\Usecases\FixedBalance\GetFixedBalancesUsecase;
use App\Usecases\FixedBalance\InsertFixedBalanceUsecase;
use App\Usecases\FixedBalance\SelectFixedBalanceUsecase;
use App\Usecases\FixedBalance\UpdateFixedBalanceUsecase;

class FixedBalanceController extends Controller
{
    public function index()
    {
        $getFixedBalancesUsecase = new GetFixedBalancesUsecase();
        $fixedBalances = $getFixedBalancesUsecase->execute();
        return array_map(
            function (FixedBalanceEntity $fixedBalance) {
                return [
                    "id" => $fixedBalance->fixedBalanceId()->value(),
                    "amount" => $fixedBalance->amount()->value(),
                    "item" => $fixedBalance->item()->value(),
                    "kind_element_id" => $fixedBalance->kindElementId()->value(),
                    "purpose_element_id" => $fixedBalance->purposeElementId()->value(),
                    "place_element_id" => $fixedBalance->placeElementId()->value(),
                    "kind_element_description" => $fixedBalance->kindElementDescription()->value(),
                    "purpose_element_description" => $fixedBalance->purposeElementDescription()->value(),
                    "place_element_description" => $fixedBalance->placeElementDescription()->value(),
                ];
            },
            $fixedBalances
        );
    }

    public function show(int $id)
    {
        $fixedBalanceId = FixedBalanceId::filledId($id);

        $selectFixedBalanceUsecase = new SelectFixedBalanceUsecase();
        $fixedBalance = $selectFixedBalanceUsecase->execute($fixedBalanceId);
        return [
            "id" => $fixedBalance->fixedBalanceId()->value(),
            "amount" => $fixedBalance->amount()->value(),
            "item" => $fixedBalance->item()->value(),
            "kind_element_id" => $fixedBalance->kindElementId()->value(),
            "purpose_element_id" => $fixedBalance->purposeElementId()->value(),
            "place_element_id" => $fixedBalance->placeElementId()->value(),
            "kind_element_description" => $fixedBalance->kindElementDescription()->value(),
            "purpose_element_description" => $fixedBalance->purposeElementDescription()->value(),
            "place_element_description" => $fixedBalance->placeElementDescription()->value(),
        ];
    }

    public function store(Request $request)
    {
        $fixedBalance = new FixedBalanceEntity(
            FixedBalanceId::emptyId(),
            new Amount($request->input("amount")),
            new Item($request->input("item")),
            KindElementId::filledId($request->input("kind_element_id")),
            PurposeElementId::filledId($request->input("purpose_element_id")),
            PlaceElementId::filledId($request->input("place_element_id")),
        );

        if ($fixedBalance->kindElementId()->isMoveId()) {
            throw new InvalidParameterException('Kind element id is move id.');
        }

        if ($fixedBalance->purposeElementId()->isMoveId()) {
            throw new InvalidParameterException('Purpose element id is move id.');
        }

        if ($fixedBalance->placeElementId()->isMoveId()) {
            throw new InvalidParameterException('Place element id is move id.');
        }

        $insertFixedBalanceUsecase = new InsertFixedBalanceUsecase();
        return $insertFixedBalanceUsecase->execute($fixedBalance);
    }

    public function update(Request $request, int $id)
    {
        $fixedBalance = new FixedBalanceEntity(
            FixedBalanceId::filledId($id),
            new Amount($request->input("amount")),
            new Item($request->input("item")),
            KindElementId::filledId($request->input("kind_element_id")),
            PurposeElementId::filledId($request->input("purpose_element_id")),
            PlaceElementId::filledId($request->input("place_element_id")),
        );

        if ($fixedBalance->kindElementId()->isMoveId()) {
            throw new InvalidParameterException('Kind element id is move id.');
        }

        if ($fixedBalance->purposeElementId()->isMoveId()) {
            throw new InvalidParameterException('Purpose element id is move id.');
        }

        if ($fixedBalance->placeElementId()->isMoveId()) {
            throw new InvalidParameterException('Place element id is move id.');
        }

        $updateFixedBalanceUsecase = new UpdateFixedBalanceUsecase();
        $updateFixedBalanceUsecase->execute($fixedBalance);
    }

    public function destroy(int $id)
    {
        $fixedBalanceId = FixedBalanceId::filledId($id);

        $deleteFixedBalanceUsecase = new DeleteFixedBalanceUsecase();
        $deleteFixedBalanceUsecase->execute($fixedBalanceId);
    }
}
