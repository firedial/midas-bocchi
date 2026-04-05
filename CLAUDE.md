# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

**midas-bocchi** は家計簿管理システム。Docker Compose で構成されたマイクロサービス構成で、サービス名はアニメキャラクターから取っている。

## Services

| Service | Role | Tech |
|---------|------|------|
| `haruhi` | API backend | Laravel 11 / PHP 8.2 / Apache |
| `nagisa` | Frontend SPA | Elm 0.19.1 |
| `konata` | Reverse proxy | Nginx |
| `yui` | Production DB | MariaDB 11.4 |
| `azusa` | Test DB | MariaDB (test init scripts) |
| `tamako` | Integration tests | PHP CLI |
| `eru` | Analytics | Metabase |
| `rikka` | Backup/restore | Alpine + OpenSSL |

## Common Commands

### Development

```bash
# Start all services
docker compose up -d

# Enter the Laravel backend
docker compose exec haruhi bash

# Run database migrations
docker compose exec haruhi php artisan migrate

# Seed the database
docker compose exec haruhi php artisan db:seed

# Build Elm frontend (outputs to konata/public/main.js)
docker compose exec nagisa elm make src/Main.elm --output=/konata/public/main.js
```

### Testing

```bash
# Run unit/feature tests (Pest) inside haruhi container
docker compose exec haruhi php artisan test

# Run a single test file
docker compose exec haruhi php artisan test tests/Feature/BalanceTest.php

# Run integration tests (tamako) with test DB
COMPOSE_FILE=compose.test.base.yaml:compose.test.dev.yaml docker compose up -d
docker compose exec tamako php /app/main.php

# Code style (Laravel Pint)
docker compose exec haruhi ./vendor/bin/pint
```

## Architecture

### Backend (haruhi) — DDD pattern

```
app/
  Http/Controllers/     # HTTP layer — thin, delegates to use cases
  Domain/
    Entities/           # Domain objects with identity (BalanceEntity, MoveEntity, etc.)
    ValueObjects/       # Immutable typed wrappers (Amount, Date, Item, IDs)
  Usecases/             # Business logic, organized by feature (Balance/, Move/, etc.)
  Infrastructure/       # Data access (Eloquent-based repositories)
  Models/DataModels/    # Eloquent models
```

Controllers call use cases; use cases operate on entities/value objects; infrastructure handles DB access. Value objects enforce type safety for domain primitives like `Amount`, `Date`, and various ID types.

### Frontend (nagisa) — Elm SPA

The Elm app handles routing internally. Compiled to a single `main.js` served by Konata. Pages map to API resources: Balance, FixedBalance, Move, AttributeElement, AttributeCategory, Salary/Bonus.

### Database users

- `midas_api` — read/write for Haruhi
- `midas_analyze` — read-only for Metabase (Eru)
- `task` — backup operations (Rikka)

### API auth

Laravel Sanctum token-based auth. All endpoints except `POST /api/login` require `Authorization: Bearer <token>`.

### Key API resources

- `/api/balances` — income/expense entries
- `/api/fixed_balances` — recurring transactions
- `/api/moves/{attribute_name}` — transactions by attribute (kind/purpose/place)
- `/api/attribute_elements/{attribute_name}` — list/create elements
- `/api/attribute_categories/{attribute_name}` — list/create categories
- `/api/salary`, `/api/bonus` — income recording
- `/api/check_place_sum` — aggregation query

### Test database

`azusa/initdb.d/` contains SQL scripts to initialize the test DB schema. `tamako/` runs end-to-end API tests against this DB.

## API Documentation

Full OpenAPI spec at [document/api/swagger.yaml](document/api/swagger.yaml).
