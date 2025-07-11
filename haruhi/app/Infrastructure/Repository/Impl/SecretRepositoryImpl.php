<?php

namespace App\Infrastructure\Repository\Impl;

use App\Infrastructure\Repository\SecretRepositoryInterface;
use App\Models\DataModels\SecretDataModel;

class SecretRepositoryImpl implements SecretRepositoryInterface
{
    public function getSecret(): array
    {
        return SecretDataModel::selectSecret();
    }

    public function putSecret(string $value): void
    {
        SecretDataModel::updateSecret($value);
    }
}
