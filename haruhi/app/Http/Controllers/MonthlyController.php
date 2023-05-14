<?php

namespace App\Http\Controllers;

use App\Service\MonthlyService;
use Illuminate\Http\Request;

class MonthlyController extends Controller
{
    public function store(Request $request)
    {
        $data = array();
        $data['houseRent'] = $request->input('house_rent');
        $data['gas'] = $request->input('gas');
        $data['water'] = $request->input('water');
        $data['elect'] = $request->input('elect');
        $data['net'] = $request->input('net');

        MonthlyService::registerMonthly($data);
    }

}
