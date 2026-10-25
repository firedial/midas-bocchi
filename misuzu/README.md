# misuzu

`haruhi` (Laravel/PHP) の API バックエンドを Haskell に移植したサービス。
既存の DB スキーマ・API 契約・エラーコードをそのまま再現しており、`tamako` の統合テスト
(全 165 ケース) が緑になることを受け入れ基準としている。

## 技術スタック

- **Servant** + **Warp** — 型レベル API 定義と HTTP サーバ
- **mysql-haskell** — 純 Haskell の MariaDB クライアント (生 SQL)
- **aeson** — JSON
- **Stack** (resolver: lts-22.43 / GHC 9.6.x)

## アーキテクチャ (DDD)

haruhi の DDD 構成をモジュールに写像している。

```
src/
  Api.hs                        ルーティング / 認証ミドルウェア / エラー整形 / E305 フォールバック
  Config.hs                     env 読み込み (DB_*, API_KEY, PORT)
  Db.hs                         接続プール + トランザクション + 値変換 + QueryException 写像
  Error.hs                      ErrorCode (E101..E999) と httpStatus、AppError
  Json.hs                       リクエスト JSON のバリデーション補助 (StrictInteger 等)
  Domain/
    ValueObjects.hs             Amount/Item/Date/Name/Priority/Id の検証 (smart constructor)
    Entities.hs                 Balance / Move / Element / Category / Template(Detail)
    Attribute.hs                kind/purpose/place と属性名パース
  Usecase/                      各リソースのトランザクション境界と存在チェック
  Infrastructure/               生 SQL リポジトリ (BalanceRepo は Move からも再利用)
  Http/                         Controller 相当。バリデーションと JSON 変換
```

## 環境変数

| 変数 | 既定 | 用途 |
|------|------|------|
| `DB_HOST` | 127.0.0.1 | MariaDB ホスト |
| `DB_PORT` | 3306 | MariaDB ポート |
| `DB_USERNAME` | root | 接続ユーザ |
| `DB_PASSWORD` | (空) | パスワード |
| `DB_DATABASE` | midas | データベース名 |
| `API_KEY` | (空) | `Authorization: Bearer <API_KEY>` の照合値 |
| `PORT` | 80 | 待ち受けポート |

## ローカルでのビルド・実行

```bash
# ビルド
stack build

# テスト DB を用意 (azusa の初期化スクリプトを流す)
docker run -d --name misuzu-testdb \
  -e MARIADB_ROOT_PASSWORD=rootpass -e MARIADB_DATABASE=midas \
  -e MARIADB_USER=midas_api -e MARIADB_PASSWORD=midaspass \
  -p 13306:3306 -v "$PWD/../azusa/initdb.d":/docker-entrypoint-initdb.d:ro \
  mariadb:11.4.10-noble --character-set-server=utf8mb4 --collation-server=utf8mb4_bin

# 起動
DB_HOST=127.0.0.1 DB_PORT=13306 DB_USERNAME=midas_api DB_PASSWORD=midaspass \
  DB_DATABASE=midas API_KEY=apikey PORT=8099 stack exec misuzu
```

## Docker / compose で tamako 統合テスト

`tamako` は `http://haruhi/api` を叩くため、`haruhi` サービスのビルドコンテキストだけ
`./misuzu` に差し替えて起動する。

```bash
COMPOSE_FILE=compose.test.base.yaml:compose.test.dev.yaml:compose.test.misuzu.yaml \
  docker compose up -d --build
docker compose exec tamako php /app/main.php
# => Tests: 165, Passed: 165, Failed: 0
```

## 本番への切り替え (未実施)

本番 `compose.yaml` の `haruhi` を misuzu に差し替える作業は別途行う。misuzu は
`DB_*` / `API_KEY` env をそのまま読むため、`haruhi` の build コンテキストを `./misuzu` に
変えるだけで置換できる。
