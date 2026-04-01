<?php

use Illuminate\Http\Request;
use Illuminate\Support\Facades\Route;
use App\Http\Controllers\AttributeCategoryController;
use App\Http\Controllers\AttributeElementController;
use App\Http\Controllers\BalanceController;
use App\Http\Controllers\FixedBalanceController;
use App\Http\Controllers\MoveController;
use App\Http\Controllers\ScenarioController;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;

/*
|--------------------------------------------------------------------------
| API Routes
|--------------------------------------------------------------------------
|
| Here is where you can register API routes for your application. These
| routes are loaded by the RouteServiceProvider within a group which
| is assigned the "api" middleware group. Enjoy building your API!
|
*/

Route::post('/login', ['as' => 'login', 'uses' => 'App\Http\Controllers\LoginController@login']);
Route::post('/logout', 'App\Http\Controllers\LoginController@logout');

Route::middleware('auth:sanctum')->group(function () {
    Route::apiResource('/balances', BalanceController::class);
    Route::apiResource('/fixed_balances', FixedBalanceController::class);
    Route::apiResource('/scenarios', ScenarioController::class);

    Route::prefix('/moves/{attribute_name}')
        ->whereIn('attribute_name', ['purposes', 'places'])
        ->controller(MoveController::class)
        ->group(function () {
            Route::get('/', 'index');
            Route::post('/', 'store');
            Route::get('/{move_id}', 'show');
            Route::put('/{move_id}', 'update');
            Route::delete('/{move_id}', 'destroy');
        });

    Route::prefix('/attribute_elements/{attribute_name}')
        ->whereIn('attribute_name', ['kind_element', 'purpose_element', 'place_element'])
        ->controller(AttributeElementController::class)
        ->group(function () {
            Route::get('/', 'index');
            Route::post('/', 'store');
            Route::get('/{element_id}', 'show');
            Route::put('/{element_id}', 'update');
        });

    Route::prefix('/attribute_categories/{attribute_name}')
        ->whereIn('attribute_name', ['kind_category', 'purpose_category', 'place_category'])
        ->controller(AttributeCategoryController::class)
        ->group(function () {
            Route::get('/', 'index');
            Route::post('/', 'store');
            Route::get('/{category_id}', 'show');
            Route::put('/{category_id}', 'update');
        });

    Route::post('/salary', 'App\Http\Controllers\SalaryController@store');
    Route::post('/bonus', 'App\Http\Controllers\BonusController@store');

    Route::post('/check_place_sum', 'App\Http\Controllers\CheckPlaceSumController@post');
});

Route::fallback(function () {
    throw new AppException(ErrorCode::PAGE_NOT_FOUND, 'Page not found.');
});
