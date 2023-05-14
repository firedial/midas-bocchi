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

Route::middleware('auth:sanctum')->get('/user', function (Request $request) {
    return $request->user();
});

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

Route::get('/attribute_elements/{element_name}', 'App\Http\Controllers\AttributeElementController@index');

Route::post('/salary', 'App\Http\Controllers\SalaryController@store');
Route::post('/bonus', 'App\Http\Controllers\BonusController@store');

Route::post('/monthly', 'App\Http\Controllers\MonthlyController@store');

