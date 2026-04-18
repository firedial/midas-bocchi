<?php

require_once __DIR__ . '/../Assert/Assert.php';
require_once __DIR__ . '/../Util/Request.php';

abstract class TestCase
{
    protected Request $request;

    public function __construct()
    {
        $this->request = $this->getAuthenticatedRequest();
    }

    protected function getAuthenticatedRequest(): Request
    {
        return new Request('apikey');
    }
}
