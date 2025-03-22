<?php

namespace App\Http\Controllers;

use App\Service\SecretService;
use App\Exceptions\InvalidParameterException;
use Illuminate\Http\Request;

class SecretController extends Controller
{
    public function get()
    {
        $secretService = new SecretService();
        return $secretService->getSecret();
    }

    public function put(Request $request)
    {
        $secret = $request->only([
            'officeTransportation',
            'insurance',
            'houseRent',
            'net',
        ]);

        // パラメータ数があっているかの確認
        if (count($secret) !== 4) {
            throw new InvalidParameterException('Parameter count is wrong.');
        }

        // 全部0以上の値が入っているかの確認
        if (count(array_filter($secret, fn($x) => is_null($x) || $x < 0)) > 0) {
            throw new InvalidParameterException('Parameter has null or minus.');
        }

        $secretService = new SecretService();
        $secretService->registerSecret($secret);
    }
}
