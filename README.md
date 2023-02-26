# midas-bocchi

## 初期設定

* リポジトリをクローンしてくる

```
$ git clone https://github.com/firedial/midas-bocchi.git
$ cd midas-bocchi
```

* compose.yaml ファイルの haruhi の command をコメントアウトする

* 一度 docker container を起動し haruhi の設定をする

```
$ docker compose up -d

$ docker compose exec haruhi bash
$ compoesr install
$ php artisan key:generate
$ exit

$ docker compose down
```

* compose.yaml ファイルの haruhi の command をコメントアウトを外す

* docker container を起動

```
$ docker compose up -d
```
