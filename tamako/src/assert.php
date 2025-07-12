<?php

class Assert
{
    private $response;
    private int $code;

    public function __construct($response, int $code)
    {
        $this->response = $response;
        $this->code = $code;
    }

    public function assertCode(int $expectedCode)
    {
        if ($this->code !== $expectedCode) {
            throw new Exception("wrong code");
        }
    }

    public function assertResponse($expectedResponse): bool
    {
        if (is_null($this->response)) {
            return is_null($expectedResponse);
        } else if (is_int($this->response)) {
            return is_int($expectedResponse) && $this->response === $expectedResponse;
        } else {
            return false;
        }
    }
}
