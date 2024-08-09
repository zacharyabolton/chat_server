# Base image
FROM erlang:alpine AS builder

# Set the working directory
WORKDIR /app/src
ENV REBAR_BASE_DIR=/app/_build

RUN rm -f /etc/apt/apt.conf.d/docker-clean

# build and cache dependencies as their own layer
COPY rebar.config rebar.lock .
RUN --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 compile

FROM builder AS compiled

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 compile

FROM compiled AS releaser

WORKDIR /app/src

# create the directory to unpack the release to
RUN mkdir -p /opt/rel

# build the release tarball and then unpack
# to be copied into the image built in the next stage
RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,target=/root/.cache/rebar3 \
    rebar3 tar && \
    tar -zxvf $REBAR_BASE_DIR/default/rel/*/*.tar.gz -C /opt/rel

# final stage
FROM erlang:alpine AS runner

WORKDIR /opt/chat_server

ENV COOKIE=chat_server \
    # write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp \
    # chat_server specific env variables to act as defaults
    DB_HOST=127.0.0.1 \
    LOGGER_LEVEL=debug \
    SBWT=none

RUN rm -f /etc/apt/apt.conf.d/docker-clean

COPY --from=releaser /opt/rel .

EXPOSE 8080

ENTRYPOINT ["/opt/chat_server/bin/chat_server"]
CMD ["foreground"]
