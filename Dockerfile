FROM haskell:9.0.2

WORKDIR /per-haskell
COPY . .
RUN stack build
RUN stack install

ENTRYPOINT ["sh", "-c", "per-haskell-exe"]
