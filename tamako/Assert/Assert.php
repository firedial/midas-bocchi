<?php

class Assert
{
    public function isStatus200(int $status)
    {
        if ($status !== 200) {
            throw new Exception('not 200');
        }
    }

    public function isStatus401(int $status)
    {
        if ($status !== 401) {
            throw new Exception('not 401');
        }
    }
}
