<?php

namespace App\Http\Controllers;

use App\Service\CheckPlaceSumService;
use App\Exceptions\InvalidParameterException;
use App\Util\DateUtil;
use Illuminate\Http\Request;

class CheckPlaceSumController extends Controller
{
    public function post(Request $request)
    {
        $data = $request->only(['placeElementId', 'date']);

        // 日付が正しい形式か
        if (!DateUtil::isValidDateString($data['date'])) {
            throw new InvalidParameterException('Date is invalid.');
        }

        $checkPlaceSumService = new CheckPlaceSumService();
        $checkPlaceSumService->registerCheckPlaceSum([
            'placeElementId' => $data['placeElementId'],
            'date' => $data['date'],
        ]);
    }

}
