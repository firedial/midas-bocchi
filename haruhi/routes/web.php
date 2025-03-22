<?php

use Illuminate\Support\Facades\Route;

/*
|--------------------------------------------------------------------------
| Web Routes
|--------------------------------------------------------------------------
|
| Here is where you can register web routes for your application. These
| routes are loaded by the RouteServiceProvider within a group which
| contains the "web" middleware group. Now create something great!
|
*/

Route::get('/{any}', function() {
        $env = match (env('APP_ENV')) {
            'production' => 'prod.css',
            'staging' => 'stag.css',
            default => 'dev.css',
        };
        return <<< EOF
<!DOCTYPE html>
<html>

<head>
	<meta charset="UTF-8">
	<title>Main</title>
	<script src="/main.js"></script>
	<link href="/$env" rel="stylesheet" type="text/css">
	<link href="/main.css" rel="stylesheet" type="text/css">
	<link href="/sanctum/csrf-cookie" rel="stylesheet" type="text/css">
</head>

<body>
	<div id="myapp"></div>
	<script>
		const xsrfToken = document.cookie
			.split("; ")
			.find((row) => row.startsWith("XSRF-TOKEN="))
			?.split("=")[1]
			.replace("%3D", "=");
		var app = Elm.Main.init({
			node: document.getElementById('myapp'),
			flags: xsrfToken
		});
	</script>
</body>

</html>
EOF;
    })->where('any', '.*');
