<?php

namespace App\Domain\ValueObjects;

use App\Exceptions\ValueObjectException;

class Date
{
    private int $year;
    private int $month;
    private int $day;

    public function __construct(string $date)
    {
        $d = explode('-', $date);
        if (count($d) !== 3) {
            throw new ValueObjectException("Date is wrong.");
        }

        $this->day = (int)$d[2];
        $this->month = (int)$d[1];
        $this->year = (int)$d[0];

        if (!checkdate($this->month, $this->day, $this->year)) {
            throw new ValueObjectException("Date is wrong.");
        }
    }

    public function value(): string
    {
        return sprintf('%d-%02d-%02d', $this->year, $this->month, $this->day);
    }

    public function year(): int
    {
        return $this->year;
    }

    public function month(): int
    {
        return $this->month;
    }

    public function day(): int
    {
        return $this->day;
    }
}
