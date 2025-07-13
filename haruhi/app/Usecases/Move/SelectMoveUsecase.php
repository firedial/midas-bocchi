<?php

namespace App\Usecases\Move;

use App\Domain\Entities\MoveEntity;
use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\MoveId;
use App\Exceptions\NotFoundException;
use App\Infrastructure\Repository\Impl\MoveRepositoryImpl;
use App\Infrastructure\Repository\MoveRepositoryInterface;
use Exception;
use Illuminate\Support\Facades\DB;

class SelectMoveUsecase
{
    private MoveRepositoryImpl $moveRepository;

    public function __construct(?MoveRepositoryInterface $moveRepository = null)
    {
        $this->moveRepository = $moveRepository ?: new MoveRepositoryImpl();
    }

    public function execute(Attribute $attribute, MoveId $moveId): MoveEntity
    {
        DB::beginTransaction();
        try {
            // 取得
            $move = $this->moveRepository->selectMove($attribute, $moveId);

            // 存在しないとき
            if (is_null($move)) {
                throw new NotFoundException("Not found move.");
            }
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $move;
    }
}
