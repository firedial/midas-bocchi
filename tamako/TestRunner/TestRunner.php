<?php

require_once __DIR__ . '/TestCase.php';

class TestRunner
{
    /** @var class-string<TestCase>[] */
    private array $testClasses = [];
    private string $host;

    public function __construct(string $host)
    {
        $this->host = $host;
    }

    /**
     * @param class-string<TestCase> $className
     */
    public function addTestClass(string $className): void
    {
        $this->testClasses[] = $className;
    }

    public function run(): void
    {
        $passed = 0;
        $failed = 0;
        $errors = [];

        foreach ($this->testClasses as $className) {
            $instance = new $className($this->host);
            $methods = get_class_methods($instance);

            foreach ($methods as $method) {
                if (str_starts_with($method, 'test')) {
                    $label = $className . '::' . $method;
                    try {
                        $instance->$method();
                        $passed++;
                        echo ".";
                    } catch (\Exception $e) {
                        $failed++;
                        echo "F";
                        $errors[] = $label . "\n    " . $e->getMessage();
                    }
                }
            }
        }

        echo "\n\n";

        if (count($errors) > 0) {
            echo "Failures:\n\n";
            foreach ($errors as $i => $error) {
                echo ($i + 1) . ") " . $error . "\n\n";
            }
        }

        $total = $passed + $failed;
        echo "Tests: {$total}, Passed: {$passed}, Failed: {$failed}\n";

        if ($failed > 0) {
            exit(1);
        }
    }
}
