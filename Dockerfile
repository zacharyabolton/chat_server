# Base image
FROM erlang:alpine AS builder

# Set the working directory
WORKDIR /app/src
ENV REBAR_BASE_DIR=/app/_build

# Remove unnecessary files
RUN rm -f /etc/apt/apt.conf.d/docker-clean

# Copy all necessary files
COPY . /app/src/

# Build and cache dependencies
RUN --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 as dev compile

# Create the directory to unpack the release to
RUN mkdir -p /opt/rel

# Build the release tarball and then unpack
RUN --mount=id=hex-cache,type=cache,target=/root/.cache/rebar3 \
    rebar3 as dev release && \
    rebar3 as dev tar && \
    tar -zxvf $REBAR_BASE_DIR/dev/rel/*/*.tar.gz -C /opt/rel

# Final stage
FROM erlang:alpine AS runner

WORKDIR /opt/chat_server

ENV COOKIE=chat_server \
    # write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp \
    # chat_server specific env variables to act as defaults
    DB_HOST=127.0.0.1 \
    LOGGER_LEVEL=debug \
    ENV=development \
    SBWT=none

RUN rm -f /etc/apt/apt.conf.d/docker-clean

COPY --from=builder /opt/rel .

ENTRYPOINT ["/opt/chat_server/bin/chat_server"]
CMD ["foreground"]
