<?php

namespace App\Usecases;

use App\Infrastructure\Repository\Impl\SecretRepositoryImpl;
use App\Infrastructure\Repository\SecretRepositoryInterface;
use Exception;
use Illuminate\Support\Facades\DB;

class PutSecretUsecase
{
    private SecretRepositoryImpl $secretRepository;

    public function __construct(
        ?SecretRepositoryInterface $secretRepository = null,
    ) {
        $this->secretRepository = $secretRepository ?: new SecretRepositoryImpl();
    }

    public function execute(array $secret): void
    {
        DB::beginTransaction();
        try {
            $secrets = $this->secretRepository->putSecret(json_encode($secret));

            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }
    }
}
