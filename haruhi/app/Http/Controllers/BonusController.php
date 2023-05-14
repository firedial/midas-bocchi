<?php

namespace App\Http\Controllers;

use App\Service\BonusService;
use Illuminate\Http\Request;

class BonusController extends Controller
{
    public function store(Request $request)
    {
        $bonus = array();
        $bonus['bonus'] = $request->input('bonus');
        $bonus['healthInsurance'] = $request->input('healthInsurance');
        $bonus['welfarePension'] = $request->input('welfarePension');
        $bonus['employmentInsurance'] = $request->input('employmentInsurance');
        $bonus['incomeTax'] = $request->input('incomeTax');
        $bonus['date'] = $request->input('date');

        BonusService::registerBonus($bonus);
    }

}
