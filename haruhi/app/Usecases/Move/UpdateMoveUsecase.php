<?php

namespace App\Usecases\Move;

use App\Domain\Entities\MoveEntity;
use App\Domain\ValueObjects\Attribute;
use App\Exceptions\NotFoundException;
use App\Infrastructure\Repository\Impl\MoveRepositoryImpl;
use App\Infrastructure\Repository\MoveRepositoryInterface;
use Exception;
use Illuminate\Support\Facades\DB;

class UpdateMoveUsecase
{
    private MoveRepositoryImpl $moveRepository;

    public function __construct(?MoveRepositoryInterface $moveRepository = null)
    {
        $this->moveRepository = $moveRepository ?: new MoveRepositoryImpl();
    }

    public function execute(Attribute $attribute, MoveEntity $move): void
    {
        DB::beginTransaction();
        try {
            $beforeMove = $this->moveRepository->selectMove($attribute, $move->moveId());

            // 存在しないとき
            if (is_null($beforeMove)) {
                throw new NotFoundException("Not found move.");
            }

            // 更新
            $this->moveRepository->updateMove($attribute, $move);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }
    }
}
