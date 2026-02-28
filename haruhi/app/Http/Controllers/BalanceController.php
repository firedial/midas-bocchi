<?php

namespace App\Http\Controllers;

use Exception;
use App\Domain\Entities\BalanceEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\BalanceId;
use Illuminate\Http\Request;
use App\Exceptions\InvalidParameterException;
use App\Domain\ValueObjects\Date;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Usecases\Balance\DeleteBalanceUsecase;
use App\Usecases\Balance\GetBalancesUsecase;
use App\Usecases\Balance\InsertBalanceUsecase;
use App\Usecases\Balance\SelectBalanceUsecase;
use App\Usecases\Balance\UpdateBalanceUsecase;

class BalanceController extends Controller
{
    public function index(Request $request)
    {
        $limit = $request->input('limit');
        if (!is_null($limit) && !is_numeric($limit)) {
            throw new InvalidParameterException('limit is wrong');
        }
        $orderby = $request->input('orderby');
        if (!is_null($orderby) && $orderby !== 'desc') {
            throw new InvalidParameterException('orderby is wrong');
        }

        $getBalancesUsecase = new GetBalancesUsecase();
        $balances = $getBalancesUsecase->execute($limit, is_null($orderby) ? null : true);
        return array_map(
            function (BalanceEntity $balance) {
                return [
                    "id" => $balance->balanceId()->value(),
                    "amount" => $balance->amount()->value(),
                    "item" => $balance->item()->value(),
                    "kind_element_id" => $balance->kindElementId()->value(),
                    "purpose_element_id" => $balance->purposeElementId()->value(),
                    "place_element_id" => $balance->placeElementId()->value(),
                    "kind_element_description" => $balance->kindElementDescription()->value(),
                    "purpose_element_description" => $balance->purposeElementDescription()->value(),
                    "place_element_description" => $balance->placeElementDescription()->value(),
                    "date" => $balance->date()->value(),
                ];
            },
            $balances
        );
    }

    public function show(int $id)
    {
        $balanceId = BalanceId::filledId($id);

        $selectBalanceUsecase = new SelectBalanceUsecase();
        $balance = $selectBalanceUsecase->execute($balanceId);
        return [
            "id" => $balance->balanceId()->value(),
            "amount" => $balance->amount()->value(),
            "item" => $balance->item()->value(),
            "kind_element_id" => $balance->kindElementId()->value(),
            "purpose_element_id" => $balance->purposeElementId()->value(),
            "place_element_id" => $balance->placeElementId()->value(),
            "kind_element_description" => $balance->kindElementDescription()->value(),
            "purpose_element_description" => $balance->purposeElementDescription()->value(),
            "place_element_description" => $balance->placeElementDescription()->value(),
            "date" => $balance->date()->value(),
        ];
    }

    public function store(Request $request)
    {
        try {
            $balance = new BalanceEntity(
                BalanceId::emptyId(),
                new Amount($request->input("amount")),
                new Item($request->input("item")),
                KindElementId::filledId($request->input("kind_element_id")),
                PurposeElementId::filledId($request->input("purpose_element_id")),
                PlaceElementId::filledId($request->input("place_element_id")),
                new Date($request->input("date")),
            );
        } catch (Exception $e) {
            throw new InvalidParameterException($e->getMessage());
        }

        if ($balance->kindElementId()->isMoveId()) {
            throw new InvalidParameterException('Kind element id is move id.');
        }

        if ($balance->purposeElementId()->isMoveId()) {
            throw new InvalidParameterException('Purpose element id is move id.');
        }

        if ($balance->placeElementId()->isMoveId()) {
            throw new InvalidParameterException('Place element id is move id.');
        }

        if ($balance->amount()->value() === 0) {
            throw new InvalidParameterException('Amount is zero.');
        }

        $insertBalanceUsecase = new InsertBalanceUsecase();
        return $insertBalanceUsecase->execute($balance);
    }

    public function update(Request $request, int $id)
    {
        try {
            $balance = new BalanceEntity(
                BalanceId::filledId($id),
                new Amount($request->input("amount")),
                new Item($request->input("item")),
                KindElementId::filledId($request->input("kind_element_id")),
                PurposeElementId::filledId($request->input("purpose_element_id")),
                PlaceElementId::filledId($request->input("place_element_id")),
                new Date($request->input("date")),
            );
        } catch (Exception $e) {
            throw new InvalidParameterException($e->getMessage());
        }

        if ($balance->kindElementId()->isMoveId()) {
            throw new InvalidParameterException('Kind element id is move id.');
        }

        if ($balance->purposeElementId()->isMoveId()) {
            throw new InvalidParameterException('Purpose element id is move id.');
        }

        if ($balance->placeElementId()->isMoveId()) {
            throw new InvalidParameterException('Place element id is move id.');
        }

        if ($balance->amount()->value() === 0) {
            throw new InvalidParameterException('Amount is zero.');
        }

        $updateBalanceUsecase = new UpdateBalanceUsecase();
        $updateBalanceUsecase->execute($balance);
    }

    public function destroy(int $id)
    {
        $balanceId = BalanceId::filledId($id);

        $deleteBalanceUsecase = new DeleteBalanceUsecase();
        $deleteBalanceUsecase->execute($balanceId);
    }
}
