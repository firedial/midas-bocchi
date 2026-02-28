<?php

class Assert
{
    public function isStatusCode200(int $status)
    {
        $this->isStatusCode($status, 200);
    }

    public function isStatusCode400(int $status)
    {
        $this->isStatusCode($status, 400);
    }

    public function isStatusCode401(int $status)
    {
        $this->isStatusCode($status, 401);
    }

    private function isStatusCode(int $status, int $code)
    {
        if ($status !== $code) {
            throw new Exception("not {$code} but {$status}");
        }
    }
}
