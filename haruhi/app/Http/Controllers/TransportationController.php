<?php

namespace App\Http\Controllers;

use App\Service\TransportationService;
use App\Exceptions\InvalidParameterException;
use App\Util\DateUtil;
use Illuminate\Http\Request;

class TransportationController extends Controller
{
    public function post(Request $request)
    {
        $data = $request->only(['date']);
        // 日付が正しい形式か
        if (!DateUtil::isValidDateString($data['date'])) {
            throw new InvalidParameterException('Date is invalid.');
        }

        $transportationService = new TransportationService();
        $transportationService->registerTransportation($data['date']);
    }

}
