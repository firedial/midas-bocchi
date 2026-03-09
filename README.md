# midas-bocchi

家計簿管理システム

## 初期設定

### git clone

```
$ git clone https://github.com/firedial/midas-bocchi.git
$ cd midas-bocchi
```

### 環境変数の設定

適宜 .env ファイルの中身を修正する。

```
$ cp .env.example .env
$ cp haruhi/.env.example haruhi/.env
```

### SSL の設定

ディレクトリの作成

```
$ mkdir -p konata/ssl
$ cd konata/ssl
```

秘密鍵の作成

```
$ openssl ecparam -genkey -name prime256v1 -noout -out server.key
```

証明書署名要求とサーバ証明書の作成


```
$ DOMAIN=midas.home.arpa openssl req -new -x509 -key server.key -out server.crt -days 365 -config openssl.cnf -extensions v3_req
```

( `openssl.cnf` は `konat/ssl/openssl.cnf` を使う)

### イメージの作成とコンテナ作成と起動

```
$ docker compose up -d
```

### haruhi の設定

haruhi コンテナの中に入って設定をする。

```
$ docker compose exec haruhi bash
$ composer install
$ php artisan key:generate

$ npm i --save-dev laravel-mix@latest &&
$ npm i --save-dev sass-loader@latest &&
$ npm i --save-dev postcss@latest &&
$ npm i --save-dev webpack@latest

$ npm run dev
```


## 永続化データ

### 新規

DB は haruhi に入ってマイグレーションする。

```
$ docker compose exec haruhi bash
$ php artisan migrate
```

必要ならシーディングもする。

```
$ php artisan db:seed
```

Metabase の設定はブラウザから行う。
https://[ドメイン名]/eru にアクセスることで設定画面が表示される。

Metabase のアカウントを設定と、DB の設定を行う。

DB の設定は .env で設定した値を入れる。
* ホスト: DB_HOST
* ポート: 3306
* ユーザ: DB_MIDAS_ANALYZE_USER
* パスワード: DB_MIDAS_ANALYZE_PASSWORD
* データベース名: DB_DATABASE

### バックアップからリストア

rikka コンテナからリストアする。それぞれ DB と Metabase のリストアコマンドになる。

```
$ docker compose exec rikka sh
$ sh /app/backup/crypt_restore.sh
$ sh /app/backup/crypt_metabase_restore.sh
```
