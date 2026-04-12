<?php

namespace App\Http\Middleware;

use Closure;
use Illuminate\Http\Request;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;

class CheckApiKey
{
    public function handle(Request $request, Closure $next): mixed
    {
        $bearer = $request->bearerToken();

        if ($bearer === null || $bearer !== env('API_KEY')) {
            throw new AppException(ErrorCode::UNAUTHORIZED, 'Unauthorized');
        }

        return $next($request);
    }
}
