<?php

namespace App\Http\Controllers;

use App\Domain\Entities\SalaryEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Date;
use App\Exceptions\InvalidParameterException;
use App\Usecases\SalaryUsecase;
use Illuminate\Http\Request;

class SalaryController extends Controller
{
    public function store(Request $request)
    {
        $salary = new SalaryEntity(
            new Amount($request->input('baseSalary')),
            new Amount($request->input('adjustmentSalary')),
            new Amount($request->input('transportation')),
            new Amount($request->input('holdingIncentives')),
            new Amount($request->input('healthInsurance')),
            new Amount($request->input('welfarePension')),
            new Amount($request->input('residentTax')),
            new Amount($request->input('employmentInsurance')),
            new Amount($request->input('incomeTax')),
            new Amount($request->input('holding')),
            new Date($request->input('date')),
        );

        if ($salary->baseSalary()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }
        if ($salary->adjustmentSalary()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }
        if ($salary->transportation()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }
        if ($salary->holdingIncentives()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }
        if ($salary->healthInsurance()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }
        if ($salary->welfarePension()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }
        if ($salary->residentTax()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }
        if ($salary->employmentInsurance()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }
        if ($salary->incomeTax()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }
        if ($salary->holding()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }

        $salaryUsecase = new SalaryUsecase();
        $salaryUsecase->execute($salary);
    }
}
