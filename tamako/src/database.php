<?php

class Database
{
    private $pdo;

    public function __construct()
    {
        $host = getenv("DB_HOST");
        $database = getenv("DB_DATABASE");
        $username = getenv("DB_USERNAME");
        $password = getenv("DB_PASSWORD");

        $this->pdo = new PDO("mysql:host=$host;dbname=$database", $username, $password);
    }

    public function get()
    {
        $stmt = $this->pdo->query("SELECT * FROM m_kind_element");
        return $stmt->fetchAll();
    }
}
