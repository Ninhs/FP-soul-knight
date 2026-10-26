FROM haskell:9.10.3

WORKDIR /app
COPY . .

RUN cabal update && cabal build server

CMD ["cabal", "run", "server"]