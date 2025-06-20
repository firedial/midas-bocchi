<?php

namespace App\Usecases\Move;

use App\Domain\ValueObjects\Attribute;
use App\Infrastructure\Repository\Impl\MoveRepositoryImpl;
use App\Infrastructure\Repository\MoveRepositoryInterface;
use Exception;
use Illuminate\Support\Facades\DB;

class GetMovesUsecase
{
    private MoveRepositoryImpl $moveRepository;

    public function __construct(?MoveRepositoryInterface $moveRepository = null)
    {
        $this->moveRepository = $moveRepository ?: new MoveRepositoryImpl();
    }

    public function execute(Attribute $attribute, ?int $limit): array
    {
        DB::beginTransaction();
        try {
            $moves = $this->moveRepository->getMoves($attribute, limit: $limit);
            DB::commit();
        } catch (Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $moves;
    }
}
