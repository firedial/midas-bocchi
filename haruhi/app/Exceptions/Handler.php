<?php

namespace App\Exceptions;

use Illuminate\Auth\AuthenticationException;
use Illuminate\Foundation\Exceptions\Handler as ExceptionHandler;
use Illuminate\Http\Request;
use Symfony\Component\HttpKernel\Exception\MethodNotAllowedHttpException;
use Throwable;

class Handler extends ExceptionHandler
{
    /**
     * A list of the exception types that are not reported.
     *
     * @var array
     */
    protected $dontReport = [
        AppException::class,
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
            if ($e instanceof AppException) {
                return response()->json([
                    'code' => $e->errorCode->value,
                    'message' => $e->getMessage(),
                ], $e->errorCode->httpStatus());
            } else if ($e instanceof AuthenticationException) {
                return response()->json([
                    'code' => ErrorCode::UNAUTHORIZED->value,
                    'message' => 'Unauthorized',
                ], 401);
            } else if ($e instanceof MethodNotAllowedHttpException) {
                return response()->json([
                    'code' => ErrorCode::PAGE_NOT_FOUND->value,
                    'message' => 'Method not allowed.',
                ], 404);
            } else {
                return response()->json([
                    'code' => ErrorCode::UNEXPECTED->value,
                    'message' => $e->getMessage(),
                ], 500);
            }
        });
    }
}
