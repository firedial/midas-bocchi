<?php

namespace App\Usecases\Move;

use App\Domain\Entities\MoveEntity;
use App\Domain\ValueObjects\Attribute;
use App\Exceptions\AppException;
use App\Exceptions\ErrorCode;
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

    public function execute(Attribute $attribute, MoveEntity $move): MoveEntity
    {
        DB::beginTransaction();
        try {
            $beforeMove = $this->moveRepository->selectMove($attribute, $move->moveId());

            // 存在しないとき
            if (is_null($beforeMove)) {
                throw new AppException(ErrorCode::RECORD_NOT_FOUND, "Not found move.");
            }

            // 更新
            $result = $this->moveRepository->updateMove($attribute, $move);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $result;
    }
}
