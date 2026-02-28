<?php

readonly class Response
{
    public function __construct(private string $rawHeader, private string $rawBody) {}

    public function statusLine(): string
    {
        // 改行コードを \n だけにしてから分割し、ステータスラインを取得する
        return explode("\n", str_replace("/r", '', $this->rawHeader))[0];
    }

    public function statusCode(): int
    {
        return (int)explode(' ', $this->statusLine())[1];
    }

    private function getHeaderSetCookies(): array
    {
        // 改行コードを \n だけにしてから分割し、ステータスラインを省く
        $records = array_slice(explode("\n", str_replace("/r", '', $this->rawHeader)), '1');

        $cookies = [];
        foreach ($records as $record) {
            $data = explode(': ', $record);
            $key = trim($data[0]);
            if ($key != 'Set-Cookie') {
                continue;
            }

            $value = str_replace($data[0] . ': ', '', $record);
            $cookies[] = trim($value);
        }

        return $cookies;
    }

    public function jsonBody(): mixed
    {
        if (is_array($this->rawBody)) {
            return json_decode($this->rawBody, true);
        } else {
            return $this->rawBody;
        }
    }

    public function getSessionKey(): string
    {
        $cookies = $this->getHeaderSetCookies();

        // laravel_session のクッキーを探す
        foreach ($cookies as $cookie) {
            if (str_starts_with($cookie, 'laravel_session')) {
                $divided = explode(';', $cookie)[0];
                return str_replace('laravel_session=', '', $divided);
            }
        }

        return '';
    }

    public function getXsrfToken(): string
    {
        $cookies = $this->getHeaderSetCookies();

        // XSRF-TOKEN のクッキーを探す
        foreach ($cookies as $cookie) {
            if (str_starts_with($cookie, 'XSRF-TOKEN')) {
                $divided = explode(';', $cookie)[0];
                return str_replace('XSRF-TOKEN=', '', $divided);
            }
        }

        return '';
    }
}
