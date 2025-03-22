<?php

namespace App\Service;

use App\Exceptions\InvalidParameterException;
use App\Models\Dao\SecretDao;
use App\Models\Dao\Impl\SecretDaoImpl;

/**
 * 秘匿情報のサービスクラス
 */
class SecretService
{
    private $secretDao;

    public function __construct(SecretDao $secretDao = null)
    {
        $this->secretDao = $secretDao ?: new SecretDaoImpl();
    }

    public function getSecret(): array
    {
        return json_decode($this->secretDao->selectSecret()[0]->value, true);
    }

    public function registerSecret(array $secret): bool
    {
        \DB::beginTransaction();
        try {
            $this->secretDao->updateSecret(['value' => json_encode($secret)]);
            \DB::commit();
        } catch (Exception $e) {
            \DB::rollback();
        }

        return true;
    }

}
