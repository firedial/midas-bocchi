<?php

class Assert
{
    public function is200(int $status)
    {
        if ($status !== 200) {
            throw new Exception('not 200');
        }
    }

    public function is401(int $status)
    {
        if ($status !== 401) {
            throw new Exception('not 401');
        }
    }
}
