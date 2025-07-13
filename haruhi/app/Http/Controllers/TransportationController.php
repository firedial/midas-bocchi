<?php

namespace App\Http\Controllers;

use App\Domain\ValueObjects\Date;
use App\Usecases\TransportationUsecase;
use Illuminate\Http\Request;

class TransportationController extends Controller
{
    public function post(Request $request)
    {
        $date = new Date($request->input("date"));

        $transportationUsecate = new TransportationUsecase();
        $transportationUsecate->execute($date);
    }
}
