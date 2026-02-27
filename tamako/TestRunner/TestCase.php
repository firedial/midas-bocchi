<?php

require_once __DIR__ . '/../Assert/Assert.php';
require_once __DIR__ . '/../Util/Request.php';

abstract class TestCase
{
    protected string $host;

    public function __construct(string $host)
    {
        $this->host = $host;
    }
}
