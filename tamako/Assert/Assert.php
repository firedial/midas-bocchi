<?php

class Assert
{
    public static function assertSame(mixed $expected, mixed $value, string $message)
    {
        if ($expected !== $value) {
            throw new Exception("[{$message}] Not {$expected} but {$value}");
        }
    }

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

    public static function assertStatusCode404(int $status)
    {
        self::assertStatusCode($status, 404);
    }

    private static function assertStatusCode(int $status, int $code)
    {
        if ($status !== $code) {
            throw new Exception("not {$code} but {$status}");
        }
    }
}
