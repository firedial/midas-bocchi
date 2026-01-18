<?php

require_once 'Response.php';

class Request
{
    private const DOMAIN = 'http://haruhi/api';

    private $ch;

    public function __construct(?string $session = null)
    {
        $this->ch = curl_init();
        curl_setopt($this->ch, CURLOPT_HEADER, true);
        curl_setopt($this->ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($this->ch, CURLOPT_SSL_VERIFYPEER, false);

        $headers = [
            'Content-Type: application/json',
            'Accept-Charset: UTF-8',
        ];

        if (!is_null($session)) {
            $headers[] = "Cookie: laravel_session=$session;";
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
        curl_setopt($this->ch, CURLOPT_URL, self::DOMAIN . $url);
        $r = curl_exec($this->ch);

        $headerSize = curl_getinfo($this->ch, CURLINFO_HEADER_SIZE);
        $header = substr($r, 0, $headerSize);
        $body = substr($r, $headerSize);

        return new Response($header, $body);
    }

    public function post(string $url, array $params): Response
    {
        curl_setopt($this->ch, CURLOPT_CUSTOMREQUEST, "POST");
        curl_setopt($this->ch, CURLOPT_URL, self::DOMAIN . $url);
        curl_setopt($this->ch, CURLOPT_POSTFIELDS, json_encode($params));
        $r = curl_exec($this->ch);

        $headerSize = curl_getinfo($this->ch, CURLINFO_HEADER_SIZE);
        $header = substr($r, 0, $headerSize);
        $body = substr($r, $headerSize);

        return new Response($header, $body);
    }
}
