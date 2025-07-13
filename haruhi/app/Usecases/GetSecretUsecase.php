<?php

namespace App\Usecases;

use App\Infrastructure\Repository\Impl\SecretRepositoryImpl;
use App\Infrastructure\Repository\SecretRepositoryInterface;
use Exception;
use Illuminate\Support\Facades\DB;

class GetSecretUsecase
{
    private SecretRepositoryImpl $secretRepository;

    public function __construct(
        ?SecretRepositoryInterface $secretRepository = null,
    ) {
        $this->secretRepository = $secretRepository ?: new SecretRepositoryImpl();
    }

    public function execute(): array
    {
        DB::beginTransaction();
        try {
            $secrets = $this->secretRepository->getSecret();

            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return count($secrets) === 0 ? [] : json_decode(((array)$secrets[0]->value)[0], true);
    }
}
