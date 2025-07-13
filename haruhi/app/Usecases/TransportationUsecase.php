<?php

namespace App\Usecases;

use App\Domain\Entities\BalanceEntity;
use App\Domain\ValueObjects\Amount;
use App\Domain\ValueObjects\BalanceId;
use App\Domain\ValueObjects\Date;
use App\Domain\ValueObjects\Item;
use App\Domain\ValueObjects\KindElementId;
use App\Domain\ValueObjects\PlaceElementId;
use App\Domain\ValueObjects\PurposeElementId;
use App\Infrastructure\Repository\BalanceRepositoryInterface;
use App\Infrastructure\Repository\Impl\BalanceRepositoryImpl;
use App\Infrastructure\Repository\Impl\SecretRepositoryImpl;
use App\Infrastructure\Repository\SecretRepositoryInterface;
use Exception;
use Illuminate\Support\Facades\DB;

class TransportationUsecase
{
    private BalanceRepositoryImpl $balanceRepository;
    private SecretRepositoryImpl $secretRepository;

    public function __construct(
        ?BalanceRepositoryInterface $balanceRepository = null,
        ?SecretRepositoryInterface $secretRepository = null,
    ) {
        $this->balanceRepository = $balanceRepository ?: new BalanceRepositoryImpl();
        $this->secretRepository = $secretRepository ?: new SecretRepositoryImpl();
    }

    public function execute(Date $date): void
    {
        DB::beginTransaction();
        try {
            $secret = $this->secretRepository->getSecret();
            // @todo ValueObject 型にする
            $amount = new Amount(json_decode($secret[0]->value, true)["officeTransportation"]);

            $transportation = new BalanceEntity(
                BalanceId::emptyId(),
                $amount,
                new Item("交通費"),
                KindElementId::officeTransportationId(),
                PurposeElementId::transportationId(),
                PlaceElementId::officeTransportationId(),
                $date,
            );
            $this->balanceRepository->insertBalance($transportation);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }
    }
}
