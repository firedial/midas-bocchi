<?php

namespace App\Http\Controllers;

use App\Service\SalaryService;
use Illuminate\Http\Request;

class SalaryController extends Controller
{
    public function store(Request $request)
    {
        $salary = array();
        $salary['baseSalary'] = $request->input('baseSalary');
        $salary['adjustmentSalary'] = $request->input('adjustmentSalary');
        $salary['transportation'] = $request->input('transportation');
        $salary['holdingIncentives'] = $request->input('holdingIncentives');
        $salary['healthInsurance'] = $request->input('healthInsurance');
        $salary['welfarePension'] = $request->input('welfarePension');
        $salary['residentTax'] = $request->input('residentTax');
        $salary['employmentInsurance'] = $request->input('employmentInsurance');
        $salary['incomeTax'] = $request->input('incomeTax');
        $salary['holding'] = $request->input('holding');
        $salary['date'] = $request->input('date');

        SalaryService::registerSalary($salary);
    }

}
