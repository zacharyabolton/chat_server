# Base image
FROM erlang:latest

# Install rebar3
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3 && mv rebar3 /usr/local/bin/

# Install inotify-tools for live reloading
RUN apt-get update && apt-get install -y inotify-tools

# Set the working directory
WORKDIR /app

# Copy the project files
COPY . .

# Install dependencies
RUN rebar3 get-deps

# Shell script to compile, start the application, and watch for changes
RUN echo '#!/bin/sh\n\
rebar3 compile; \n\
rebar3 shell & \n\
while true; do \n\
  inotifywait -e modify,create,delete -r src; \n\
  rebar3 compile; \n\
  pkill -f "beam.smp"; \n\
  rebar3 shell & \n\
done' > /watch.sh

RUN chmod +x /watch.sh

# Command to run the application with live reloading
CMD ["sh", "/watch.sh"]
