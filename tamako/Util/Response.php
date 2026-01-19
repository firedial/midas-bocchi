<?php

readonly class Response
{
    public function __construct(private string $rawHeader, private string $rawBody) {}

    public function statusLine(): string
    {
        // 改行コードを \n だけにしてから分割し、ステータスラインを取得する
        return explode("\n", str_replace("/r", '', $this->rawHeader))[0];
    }

    public function status(): int
    {
        return (int)explode(' ', $this->statusLine())[1];
    }

    public function headers(): array
    {
        // 改行コードを \n だけにしてから分割し、ステータスラインを省く
        $records = array_slice(explode("\n", str_replace("/r", '', $this->rawHeader)), '1');

        $headers = [];
        foreach ($records as $record) {
            echo $record;
            $data = explode(': ', $record);
            $key = trim($data[0]);
            if ($key == '') {
                continue;
            }

            $value = str_replace($data[0] . ': ', '', $record);
            $headers[$key] = trim($value);
        }

        return $headers;
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
        $headers = $this->headers();
        $cookies = explode(';', $headers['Set-Cookie']);

        // laravel_session のクッキーを探す
        foreach ($cookies as $cookie) {
            if (str_starts_with($cookie, 'laravel_session')) {
                break;
            }
        }

        return str_replace('laravel_session=', '', $cookie);
    }
}
