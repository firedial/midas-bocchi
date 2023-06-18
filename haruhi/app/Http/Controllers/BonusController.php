<?php

namespace App\Http\Controllers;

use App\Service\BonusService;
use Illuminate\Http\Request;
use App\Exceptions\InvalidParameterException;
use App\Util\DateUtil;

class BonusController extends Controller
{
    public function store(Request $request)
    {
        $bonus = $request->only([
            'bonus',
            'healthInsurance',
            'welfarePension',
            'employmentInsurance',
            'incomeTax',
            'date',
        ]);

        // パラメータ数があっているかの確認
        if (count($bonus) !== 6) {
            throw new InvalidParameterException('Parameter count is wrong.');
        }

        // 全部0以上の値が入っているかの確認
        if (count(array_filter($bonus, fn($x) => is_null($x) || $x < 0)) > 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }

        // 日付の形式があっているかの確認
        if (!DateUtil::isValidDateString($bonus['date'])) {
            throw new InvalidParameterException('Date is invalid.');
        }

        $bonusService = new BonusService();
        $bonusService->registerBonus($bonus);
    }

}
