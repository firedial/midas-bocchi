<?php

require_once __DIR__ . '/Response.php';

class Request
{
    private $ch;
    private string $appUrl;

    public function __construct(?string $session = null, ?string $xsrfToken = null)
    {
        $this->appUrl = getenv('APP_URL');

        $this->ch = curl_init();
        curl_setopt($this->ch, CURLOPT_HEADER, true);
        curl_setopt($this->ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($this->ch, CURLOPT_SSL_VERIFYPEER, false);

        $headers = [
            'Content-Type: application/json',
            'Accept-Charset: UTF-8',
        ];

        $cookies = [];
        if (!is_null($session)) {
            $cookies[] = "laravel_session=$session;";
            $cookies[] = "XSRF-TOKEN=$xsrfToken";
        }

        if (count($cookies) != 0) {
            $headers[] = "Cookie: " . implode(';', $cookies);
        }
        curl_setopt($this->ch, CURLOPT_HTTPHEADER, $headers);
    }

    public function __destruct()
    {
        unset($this->ch);
    }

    public function get(string $url): Response
    {
        curl_setopt($this->ch, CURLOPT_CUSTOMREQUEST, "GET");
        curl_setopt($this->ch, CURLOPT_URL, $this->appUrl . $url);
        $r = curl_exec($this->ch);

        $headerSize = curl_getinfo($this->ch, CURLINFO_HEADER_SIZE);
        $header = substr($r, 0, $headerSize);
        $body = substr($r, $headerSize);

        return new Response($header, $body);
    }

    public function post(string $url, array $params): Response
    {
        curl_setopt($this->ch, CURLOPT_CUSTOMREQUEST, "POST");
        curl_setopt($this->ch, CURLOPT_URL, $this->appUrl . $url);
        curl_setopt($this->ch, CURLOPT_POSTFIELDS, json_encode($params));
        $r = curl_exec($this->ch);

        $headerSize = curl_getinfo($this->ch, CURLINFO_HEADER_SIZE);
        $header = substr($r, 0, $headerSize);
        $body = substr($r, $headerSize);

        return new Response($header, $body);
    }

    public function put(string $url, array $params): Response
    {
        curl_setopt($this->ch, CURLOPT_CUSTOMREQUEST, "PUT");
        curl_setopt($this->ch, CURLOPT_URL, $this->appUrl . $url);
        curl_setopt($this->ch, CURLOPT_POSTFIELDS, json_encode($params));
        $r = curl_exec($this->ch);

        $headerSize = curl_getinfo($this->ch, CURLINFO_HEADER_SIZE);
        $header = substr($r, 0, $headerSize);
        $body = substr($r, $headerSize);

        return new Response($header, $body);
    }

    public function delete(string $url): Response
    {
        curl_setopt($this->ch, CURLOPT_CUSTOMREQUEST, "DELETE");
        curl_setopt($this->ch, CURLOPT_URL, $this->appUrl . $url);
        $r = curl_exec($this->ch);

        $headerSize = curl_getinfo($this->ch, CURLINFO_HEADER_SIZE);
        $header = substr($r, 0, $headerSize);
        $body = substr($r, $headerSize);

        return new Response($header, $body);
    }
}
