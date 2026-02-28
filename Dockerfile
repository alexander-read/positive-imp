# ---------- Stage 1: Build base ----------
FROM haskell:9.4.7 AS base

WORKDIR /app

# Cache setup: install tools, dependencies
COPY stack.yaml package.yaml ./
RUN stack setup && stack build --only-dependencies

# ---------- Stage 2: Compile ----------
FROM base AS builder

COPY . .
RUN stack build --copy-bins --local-bin-path /app/bin

# ---------- Stage 3: Runtime ----------
FROM debian:bullseye-slim

WORKDIR /app

COPY --from=builder /app/bin/positive-imp-exe .

RUN apt-get update && apt-get install -y libgmp10 locales && rm -rf /var/lib/apt/lists/* && sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

CMD ["./positive-imp-exe"]
