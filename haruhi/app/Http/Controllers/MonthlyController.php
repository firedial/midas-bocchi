<?php

namespace App\Http\Controllers;

use App\Domain\Entities\MonthlyPaymentEntity;
use App\Usecases\MonthlyUsecase;
use Illuminate\Http\Request;

class MonthlyController extends Controller
{
    public function store(Request $request)
    {
        $monthlyUsecase = new MonthlyUsecase();
        $monthlyUsecase->execute(
            new MonthlyPaymentEntity($request->input('house_rent')),
            new MonthlyPaymentEntity($request->input('gas')),
            new MonthlyPaymentEntity($request->input('water')),
            new MonthlyPaymentEntity($request->input('elect')),
            new MonthlyPaymentEntity($request->input('net')),
        );
    }
}
