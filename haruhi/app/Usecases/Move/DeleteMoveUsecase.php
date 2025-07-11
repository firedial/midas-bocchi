<?php

namespace App\Usecases\Move;

use App\Domain\ValueObjects\Attribute;
use App\Domain\ValueObjects\MoveId;
use App\Exceptions\NotFoundException;
use App\Infrastructure\Repository\Impl\MoveRepositoryImpl;
use App\Infrastructure\Repository\MoveRepositoryInterface;
use Exception;
use Illuminate\Support\Facades\DB;

class DeleteMoveUsecase
{
    private MoveRepositoryImpl $moveRepository;

    public function __construct(?MoveRepositoryInterface $moveRepository = null)
    {
        $this->moveRepository = $moveRepository ?: new MoveRepositoryImpl();
    }

    public function execute(Attribute $attribute, MoveId $moveId): void
    {
        DB::beginTransaction();
        try {
            $beforeMove = $this->moveRepository->selectMove($attribute, $moveId);

            // 存在しないとき
            if (is_null($beforeMove)) {
                throw new NotFoundException("Not found move.");
            }

            // 削除
            $this->moveRepository->deleteMove($moveId);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }
    }
}
