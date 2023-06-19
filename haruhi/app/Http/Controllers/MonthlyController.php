<?php

namespace App\Http\Controllers;

use App\Service\MonthlyService;
use App\Exceptions\InvalidParameterException;
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

        $params = ['houseRent', 'gas', 'water', 'elect', 'net'];
        foreach ($params as $param) {
            if ($data[$param]['amount'] < 0) {
                throw new InvalidParameterException("{$param} is minus.");
            }
            if ($data[$param]['amount'] > 0 && empty($data[$param]['date'])) {
                throw new InvalidParameterException("{$param} date is empty.");
            }
        }

        $monthlyService = new MonthlyService();
        $monthlyService->registerMonthly($data);
    }

}
