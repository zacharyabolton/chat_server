# Base image
FROM erlang:alpine AS builder

# Set the working directory
WORKDIR /app/src
ENV REBAR_BASE_DIR=/app/_build

# Remove unnecessary files
RUN rm -f /etc/apt/apt.conf.d/docker-clean

# Copy all necessary files, including tests
COPY . /app/src/
# build and cache dependencies as their own layer
RUN --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 as dev compile

FROM builder AS compiled

# RUN --mount=target=. \
#     --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
#     rebar3 as dev compile

# No need to compile again, just inherit compiled artifacts
FROM compiled AS releaser

WORKDIR /app/src

# Create the directory to unpack the release to
RUN mkdir -p /opt/rel

# Build the release tarball and then unpack
# to be copied into the image built in the next stage
RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,target=/root/.cache/rebar3 \
    rebar3 as dev tar && \
    tar -zxvf $REBAR_BASE_DIR/dev/rel/*/*.tar.gz -C /opt/rel

# Final stage
FROM erlang:alpine AS runner

WORKDIR /opt/chat_server


ENV COOKIE=chat_server \
    # Write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp \
    # Default environment variables
    DB_HOST=127.0.0.1 \
    LOGGER_LEVEL=debug \
    ENV=development \
    SBWT=none

# Copy release files
COPY --from=releaser /opt/rel .

# Adjust entrypoint to use shell if needed for debugging
ENTRYPOINT ["/opt/chat_server/bin/chat_server"]
# CMD ["/bin/sh"]

# Ensure that the entrypoint script can execute rebar3 commands
CMD ["foreground"]
