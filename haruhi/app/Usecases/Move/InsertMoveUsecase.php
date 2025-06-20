<?php

namespace App\Usecases\Move;

use App\Domain\Entities\MoveEntity;
use App\Domain\ValueObjects\Attribute;
use App\Infrastructure\Repository\Impl\MoveRepositoryImpl;
use App\Infrastructure\Repository\MoveRepositoryInterface;
use Exception;
use Illuminate\Support\Facades\DB;

class InsertMoveUsecase
{
    private MoveRepositoryImpl $moveRepository;

    public function __construct(?MoveRepositoryInterface $moveRepository = null)
    {
        $this->moveRepository = $moveRepository ?: new MoveRepositoryImpl();
    }

    public function execute(Attribute $attribute, MoveEntity $move): int
    {
        DB::beginTransaction();
        try {
            // 挿入
            $insertId = $this->moveRepository->insertMove($attribute, $move);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $insertId;
    }
}
