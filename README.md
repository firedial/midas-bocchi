# midas-bocchi

家計簿管理システム

## 初期設定

```
git clone https://github.com/firedial/midas-bocchi.git
cd midas-bocchi

cp .env.example .env
cp haruhi/.env.example haruhi/.env

docker compose up -d

docker compose exec haruhi bash
composer install
php artisan key:generate

npm i --save-dev laravel-mix@latest &&
npm i --save-dev sass-loader@latest &&
npm i --save-dev postcss@latest &&
npm i --save-dev webpack@latest

npm run dev
```



