<?php

use Illuminate\Http\Request;
use Illuminate\Support\Facades\Route;

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

Route::middleware('auth:sanctum')->group(function() {
    Route::get('/balances', 'App\Http\Controllers\BalanceController@index');
    Route::post('/balances', 'App\Http\Controllers\BalanceController@store');
    Route::get('/balances/{balance}', 'App\Http\Controllers\BalanceController@show');
    Route::put('/balances/{balance}', 'App\Http\Controllers\BalanceController@update');
    Route::delete('/balances/{balance}', 'App\Http\Controllers\BalanceController@destroy');

    Route::get('/moves/{attribute_name}', 'App\Http\Controllers\MoveController@index');
    Route::post('/moves/{attribute_name}', 'App\Http\Controllers\MoveController@store');
    Route::get('/moves/{attribute_name}/{move_id}', 'App\Http\Controllers\MoveController@show');
    Route::put('/moves/{attribute_name}/{move_id}', 'App\Http\Controllers\MoveController@update');
    Route::delete('/moves/{attribute_name}/{move_id}', 'App\Http\Controllers\MoveController@destroy');

    Route::get('/attribute_elements/{attribute_name}', 'App\Http\Controllers\AttributeElementController@index');
    Route::post('/attribute_elements/{attribute_name}', 'App\Http\Controllers\AttributeElementController@store');
    Route::get('/attribute_elements/{attribute_name}/{element_id}', 'App\Http\Controllers\AttributeElementController@show');
    Route::put('/attribute_elements/{attribute_name}/{element_id}', 'App\Http\Controllers\AttributeElementController@update');

    Route::get('/attribute_categories/{attribute_name}', 'App\Http\Controllers\AttributeCategoryController@index');
    Route::post('/attribute_categories/{attribute_name}', 'App\Http\Controllers\AttributeCategoryController@store');
    Route::put('/attribute_categories/{attribute_name}/{category_id}', 'App\Http\Controllers\AttributeCategoryController@update');

    Route::post('/salary', 'App\Http\Controllers\SalaryController@store');
    Route::post('/bonus', 'App\Http\Controllers\BonusController@store');

    Route::post('/monthly', 'App\Http\Controllers\MonthlyController@store');
    Route::post('/transportation', 'App\Http\Controllers\TransportationController@post');

    Route::get('/secret', 'App\Http\Controllers\SecretController@get');
    Route::put('/secret', 'App\Http\Controllers\SecretController@put');
});
