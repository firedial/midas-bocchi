<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use Illuminate\Support\Facades\Auth;
use App\Models\User;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;
use Illuminate\Validation\ValidationException;

class LoginController extends Controller
{
    /**
     * @param  Request  $request
     * @return \Illuminate\Http\JsonResponse
     */
    public function login(Request $request)
    {
        try {
            $credentials = $request->validate([
                'email' => ['required', 'email'],
                'password' => ['required'],
            ]);
        } catch (ValidationException $e) {
            $failed = $e->validator->failed();

            if (isset($failed['email']['Required'])) {
                throw new AppException(ErrorCode::MISSING_REQUIRED, 'email is required');
            }
            if (isset($failed['email']['Email'])) {
                throw new AppException(ErrorCode::INVALID_FORMAT, 'email format is invalid');
            }
            if (isset($failed['password']['Required'])) {
                throw new AppException(ErrorCode::MISSING_REQUIRED, 'password is required');
            }

            throw $e;
        }

        if (Auth::attempt($credentials)) {
            $request->session()->regenerate();
            $user = User::whereEmail($request->email)->first();

            Auth::login($user, $remember = true);
            return response()->json([], 200);
        } else {
            throw new AppException(ErrorCode::UNAUTHORIZED, 'Unauthorized');
        }
    }

    /**
     * @param  Request  $request
     * @return \Illuminate\Http\JsonResponse
     */
    public function logout(Request $request)
    {
        Auth::logout();

        $request->session()->invalidate();

        $request->session()->regenerateToken();

        return response()->json(true);
    }
}
