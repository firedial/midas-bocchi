<?php

require_once __DIR__ . '/TestRunner/TestRunner.php';
require_once __DIR__ . '/Tests/LoginTest.php';
require_once __DIR__ . '/Tests/BalanceTest.php';
require_once __DIR__ . '/Tests/MoveTest.php';
require_once __DIR__ . '/Tests/AttributeElementTest.php';
require_once __DIR__ . '/Tests/AttributeCategoryTest.php';
require_once __DIR__ . '/Tests/OtherTest.php';

$host = getenv('APP_URL') ?: 'http://haruhi/api';

$runner = new TestRunner($host);

$runner->addTestClass(LoginTest::class);
$runner->addTestClass(BalanceTest::class);
$runner->addTestClass(MoveTest::class);
$runner->addTestClass(AttributeElementTest::class);
$runner->addTestClass(AttributeCategoryTest::class);
$runner->addTestClass(OtherTest::class);

$runner->run();
