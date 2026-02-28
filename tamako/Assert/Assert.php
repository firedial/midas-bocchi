<?php

class Assert
{
    public static function assertStatusCode200(int $status)
    {
        self::assertStatusCode($status, 200);
    }

    public static function assertStatusCode400(int $status)
    {
        self::assertStatusCode($status, 400);
    }

    public static function assertStatusCode401(int $status)
    {
        self::assertStatusCode($status, 401);
    }

    private static function assertStatusCode(int $status, int $code)
    {
        if ($status !== $code) {
            throw new Exception("not {$code} but {$status}");
        }
    }
}
