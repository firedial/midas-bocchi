<?php

namespace App\Http\Controllers;

use App\Domain\Entities\BonusEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\Date;
use Illuminate\Http\Request;
use App\Exceptions\InvalidParameterException;
use App\Usecases\BonusUsecase;

class BonusController extends Controller
{
    public function store(Request $request)
    {
        $bonus = new BonusEntity(
            new Amount($request->input('bonus')),
            new Amount($request->input('healthInsurance')),
            new Amount($request->input('welfarePension')),
            new Amount($request->input('employmentInsurance')),
            new Amount($request->input('incomeTax')),
            new Date($request->input('date')),
        );

        if ($bonus->bonus()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }
        if ($bonus->healthInsurance()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }
        if ($bonus->welfarePension()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }
        if ($bonus->employmentInsurance()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }
        if ($bonus->incomeTax()->value() < 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }

        $bonusUsecase = new BonusUsecase();
        $bonusUsecase->execute($bonus);
    }
}
