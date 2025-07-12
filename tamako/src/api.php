<?php

class Api
{
    private string $baseUrl;

    public function __construct()
    {
        $domain = getenv("APP_DOMAIN");
        $this->baseUrl = "http://$domain/api";
    }

    public function get()
    {
        $path = "/attribute_categories/kind_category";

        $ch = curl_init();
        curl_setopt($ch, CURLOPT_URL, $this->baseUrl . $path);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        $r = curl_exec($ch);
        curl_close($ch);

        return $r;
    }
}
