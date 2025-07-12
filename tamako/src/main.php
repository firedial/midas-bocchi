<?php

require_once "./database.php";
require_once "./api.php";

$database = new Database();
var_dump($database->get());

$api = new Api();
var_dump($api->get());
