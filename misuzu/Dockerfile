# ---- build stage ----
FROM haskell:9.6.7 AS build

WORKDIR /build

# 依存だけ先にビルドしてレイヤキャッシュを効かせる
COPY stack.yaml stack.yaml.lock misuzu.cabal ./
RUN stack build --only-dependencies

# 本体をビルドしてバイナリを /out に出す
COPY . .
RUN stack build --copy-bins --local-bin-path /out

# ---- runtime stage ----
FROM debian:bookworm-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates libgmp10 libffi8 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /out/misuzu /usr/local/bin/misuzu

EXPOSE 80

CMD ["misuzu"]
