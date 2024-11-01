<?php

namespace App\Domain\ValueObject;

class Date
{
    private int $year;
    private int $month;
    private int $day;

    public function __construct(string $date)
    {
        $d = explode('-', $date);
        if (count($d) !== 3) {
            return false;
        }

        $day = (int)$d[2];
        $month = (int)$d[1];
        $year = (int)$d[0];

        if (!checkdate($month, $day, $year)) {
            throw new Exception("wrong date");
        }

        $this->year = $year;
        $this->month = $month;
        $this->day = $day;
    }

    public function value(): string
    {
        return $this->year . "-" . $this->month . "-" . $this->day;
    }
}
