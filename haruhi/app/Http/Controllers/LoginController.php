<?php

namespace App\Http\Controllers;

use Exception;
use Illuminate\Http\Request;
use Illuminate\Support\Facades\Auth;
use App\Models\User;
use App\Exceptions\InvalidParameterException;

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
        } catch (Exception $e) {
            throw new InvalidParameterException($e->getMessage());
        }

        if (Auth::attempt($credentials)) {
            $request->session()->regenerate();
            $user = User::whereEmail($request->email)->first();

            Auth::login($user, $remember = true);
            return response()->json([], 200);
        }
        return response()->json([], 401);
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
