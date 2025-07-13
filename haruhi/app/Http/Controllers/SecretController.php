<?php

namespace App\Http\Controllers;

use App\Usecases\GetSecretUsecase;
use App\Usecases\PutSecretUsecase;
use Illuminate\Http\Request;

class SecretController extends Controller
{
    public function get()
    {
        $getSecretUsecase = new GetSecretUsecase();
        $secret = $getSecretUsecase->execute();

        return [
            'houseRent' => $secret['houseRent'] ?? 0,
            'insurance' => $secret['insurance'] ?? 0,
            'net' => $secret['net'] ?? 0,
            'officeTransportation' => $secret['officeTransportation'] ?? 0,
        ];
    }

    public function put(Request $request)
    {
        $putSecretUsecase = new PutSecretUsecase();
        $putSecretUsecase->execute([
            'houseRent' => $request->input('houseRent'),
            'insurance' => $request->input('insurance'),
            'net' => $request->input('net'),
            'officeTransportation' => $request->input('officeTransportation'),
        ]);
    }
}
