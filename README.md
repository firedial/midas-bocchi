# midas-tari

家計簿管理システム

## 初期設定

```
git clone --recursive https://github.com/firedial/midas-tari.git 
cd midas-tari

cp haruhi/.env.example haruhi/.env
cp yui/.env.example yui/.env
cp rikka/.env.example rikka/.env
cp rikka/.encryption_key.example rikka/.encryption_key

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



