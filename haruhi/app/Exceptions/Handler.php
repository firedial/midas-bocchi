<?php

namespace App\Exceptions;

use Illuminate\Foundation\Exceptions\Handler as ExceptionHandler;
use Illuminate\Auth\AuthenticationException;
use Illuminate\Http\Request;
use Throwable;
use App\Exceptions\InvalidParameterException;
use App\Exceptions\NotFoundException;

class Handler extends ExceptionHandler
{
    /**
     * A list of the exception types that are not reported.
     *
     * @var array
     */
    protected $dontReport = [
        //
    ];

    /**
     * A list of the inputs that are never flashed for validation exceptions.
     *
     * @var array
     */
    protected $dontFlash = [
        'current_password',
        'password',
        'password_confirmation',
    ];

    /**
     * Register the exception handling callbacks for the application.
     *
     * @return void
     */
    public function register()
    {
        $this->reportable(function (Throwable $e) {
            //
        });

        $this->renderable(function (Throwable $e, Request $request) {
            if ($e instanceof InvalidParameterException) {
                return response()->json([
                    'message' => $e->getMessage()
                ], 400);
            } else if ($e instanceof NotFoundException) {
                return response()->make("", 404);
            } else if ($e instanceof AuthenticationException) {
                return response()->json([
                    'message' => $e->getMessage()
                ], 401);
            } else {
                return response()->json([
                    'message' => $e->getMessage()
                ], 500);
            }
        });
    }
}
