<?php

namespace App\Http\Controllers;

use App\Service\SalaryService;
use App\Util\DateUtil;
use App\Exceptions\InvalidParameterException;
use Illuminate\Http\Request;

class SalaryController extends Controller
{
    public function store(Request $request)
    {
        $salary = $request->only([
            'baseSalary',
            'adjustmentSalary',
            'transportation',
            'holdingIncentives',
            'healthInsurance',
            'welfarePension',
            'residentTax',
            'employmentInsurance',
            'incomeTax',
            'holding',
            'date',
        ]);

        // パラメータ数があっているかの確認
        if (count($salary) !== 11) {
            throw new InvalidParameterException('Parameter count is wrong.');
        }

        // 全部0以上の値が入っているかの確認
        if (count(array_filter($salary, fn($x) => is_null($x) || $x < 0)) > 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }

        // 日付の形式があっているかの確認
        if (!DateUtil::isValidDateString($salary['date'])) {
            throw new InvalidParameterException('Date is invalid.');
        }

        SalaryService::registerSalary($salary);
    }

}
