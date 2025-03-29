<?php

namespace App\Service;

use App\Models\Dao\BalanceDao;
use App\Models\Dao\Impl\BalanceDaoImpl;
use App\Models\Dao\MoveDao;
use App\Models\Dao\Impl\MoveDaoImpl;
use App\Exceptions\InternalException;
use App\Models\KindElement;
use App\Models\PurposeElement;
use App\Models\PlaceElement;
use Illuminate\Support\Facades\DB;

/**
 * 移動操作のサービスクラス
 */
class MoveService
{

    private $attributeName;
    private $balanceDao;
    private $moveDao;

    public function __construct(string $attributeName, BalanceDao $balanceDao = null, MoveDao $moveDao = null)
    {
        $this->attributeName = match ($attributeName) {
            'purposes' => 'purpose',
            'places' => 'place',
            default => throw new InternalException("Attribute name {$attributeName} is invalid."),
        };

        $this->balanceDao = $balanceDao ?: new BalanceDaoImpl();
        $this->moveDao = $moveDao ?: new MoveDaoImpl();
    }

    public function index(): array
    {
        return $this->moveDao->getMoves($this->attributeName);
    }

    public function show(int $id)
    {
        return $this->moveDao->getMoveById($this->attributeName, $id);
    }

    public function store(array $move)
    {
        [$before, $after] = $this->getBalancesFromMove($move);

        try {
            DB::beginTransaction();
            $this->balanceDao->insertBalance($before);
            $beforeId = (int)DB::getPdo()->lastInsertId();

            $this->balanceDao->insertBalance($after);
            $afterId = (int)DB::getPdo()->lastInsertId();

            if ($beforeId + 1 !== $afterId) {
                throw new InternalException("Move insert wrong.");
            }
            DB::commit();
        } catch (\Exception $e) {
            DB::rollBack();
            throw $e;
        }

        return $beforeId;
    }

    public function update(array $move, int $id)
    {
        [$before, $after] = $this->getBalancesFromMove($move);
        $before['id'] = $id;
        $after['id'] = $id + 1;

        // @todo 本当に移動処理かどうかのチェックをしたほうがいい
        return DB::transaction(function () use ($id, $before, $after) {
            $this->balanceDao->updateBalance($before);
            $this->balanceDao->updateBalance($after);
        });
    }

    public function destroy(int $id)
    {
        // @todo 本当に移動処理かどうかのチェックをしたほうがいい
        return DB::transaction(function () use ($id) {
            $this->balanceDao->deleteBalance($id);
            $this->balanceDao->deleteBalance($id + 1);
        });
    }

    private function getBalancesFromMove(array $move): array
    {
        $before = array(
            'item' => $move['item'],
            'amount' => (-1) * $move['amount'],
            'kind_element_id' => KindElement::MOVE_ID,
            'date' => $move['date']
        );

        $after = array(
            'item' => $move['item'],
            'amount' => $move['amount'],
            'kind_element_id' => KindElement::MOVE_ID,
            'date' => $move['date']
        );

        // @todo 移動処理が選択されていないか確認する
        if ($this->attributeName === 'purpose') {
            $before['purpose_element_id'] = $move['beforeId'];
            $before['place_element_id'] = PlaceElement::MOVE_ID;
            $after['purpose_element_id'] = $move['afterId'];
            $after['place_element_id'] = PlaceElement::MOVE_ID;
        } else if ($this->attributeName === 'place') {
            $before['purpose_element_id'] = PurposeElement::MOVE_ID;
            $before['place_element_id'] = $move['beforeId'];
            $after['purpose_element_id'] = PurposeElement::MOVE_ID;
            $after['place_element_id'] = $move['afterId'];
        } else {
            throw new InternalException("Attribute name {$this->attributeName} is invalid at store.");
        }

        return [$before, $after];
    }
}
