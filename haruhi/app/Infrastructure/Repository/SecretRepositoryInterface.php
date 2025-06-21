<?php

namespace App\Infrastructure\Repository;

interface SecretRepositoryInterface
{
    public function getSecret(): array;
    public function putSecret(string $value): void;
}
