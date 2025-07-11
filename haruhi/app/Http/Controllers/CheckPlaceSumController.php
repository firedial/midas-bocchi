<?php

namespace App\Http\Controllers;

use App\Domain\ValueObjects\Date;
use App\Domain\ValueObjects\PlaceElementId;
use App\Usecases\CheckPlaceSumUsecase;
use Illuminate\Http\Request;

class CheckPlaceSumController extends Controller
{
    public function post(Request $request)
    {
        $placeElementId = PlaceElementId::filledId($request->input("placeElementId"));
        $date = new Date($request->input("date"));

        $checkPlaceSumUsecase = new CheckPlaceSumUsecase();
        $checkPlaceSumUsecase->execute($placeElementId, $date);
    }
}
