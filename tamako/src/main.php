<?php

require_once "./database.php";
require_once "./api.php";
require_once "./util.php";

$database = new Database();
# var_dump($database->get());

$api = new Api();
var_dump($api->get("/attribute_categories/place_category"));

for ($i = 2; $i <= 100; $i++) {
    $response = $api->post("/attribute_categories/kind_category", [
        "name" => "kind_category_$i",
        "description" => "kind_category_desc_$i",
    ]);
    $response->assertCode(200);
    $response->assertResponse($i);
    $response = $api->post("/attribute_categories/purpose_category", [
        "name" => "purpose_category_$i",
        "description" => "purpose_category_desc_$i",
    ]);
    $response->assertCode(200);
    $response = $api->post("/attribute_categories/place_category", [
        "name" => "place_category_$i",
        "description" => "place_category_desc_$i",
    ]);
    $response->assertCode(200);
}

// $elementData = ["name" => "hoge", "description" => "hogehoge", "priority" => 2, "category_id" => 2];
// var_dump($api->post("/attribute_elements/kind_element", $data));
// var_dump($api->post("/attribute_elements/purpose_element", $data));
// var_dump($api->post("/attribute_elements/place_element", $data));

// var_dump($api->get("/attribute_elements/kind_element"));
// var_dump($api->get("/attribute_elements/purpose_element"));
// var_dump($api->get("/attribute_elements/place_element"));

// var_dump($api->put("/attribute_categories/kind_category/102", ["name" => "Hoge1", "description" => "hogehoge"]));
// var_dump($api->delete("/attribute_categories/kind_category/102"));

// var_dump($api->post("/balances", [
//     "amount" => "-500",
//     "item" => "hoge",
//     "kind_element_id" => "110",
//     "purpose_element_id" => "103",
//     "place_element_id" => "103",
//     "date" => "2025-05-01",
// ]));

// var_dump($api->delete("/balances/219"));

// var_dump(test([], [], 300));
