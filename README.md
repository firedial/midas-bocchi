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
$ sudo openssl genrsa -out server.key 2048
```

証明書署名要求の作成

```
$ sudo openssl req -new -key server.key -out server.csr
```

証明書署名要求の設定例

```
Country Name (2 letter code) [AU]:JP
State or Province Name (full name) [Some-State]:Tokyo
Locality Name (eg, city) []:
Organization Name (eg, company) [Internet Widgits Pty Ltd]:
Organizational Unit Name (eg, section) []:
Common Name (e.g. server FQDN or YOUR name) []:midas.home.arpa
Email Address []:

Please enter the following 'extra' attributes
to be sent with your certificate request
A challenge password []:
An optional company name []:
```

サーバ証明書の作成
```
$ sudo openssl x509 -days 3650 -req -signkey server.key -in server.csr -out server.crt
```

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
