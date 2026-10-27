FROM haskell:9.10.3-slim AS build
WORKDIR /app

RUN apt-get update && apt-get install -y libpq-dev pkg-config zlib1g-dev libzstd-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy file .cabal trước để cache layer dependency
COPY cabal.project cabal.project
COPY shared/shared.cabal shared/shared.cabal
COPY server/server.cabal server/server.cabal
COPY client/client.cabal client/client.cabal

RUN cabal update

COPY shared shared
COPY server server

RUN cabal build server

RUN mkdir -p /app/bin && cp "$(cabal list-bin server)" /app/bin/server

FROM debian:bookworm-slim
WORKDIR /app
RUN apt-get update && apt-get install -y libpq5 ca-certificates \
    && rm -rf /var/lib/apt/lists/*
COPY --from=build /app/bin/server /app/server

CMD ["/app/server"]