# Chat Server

<div style="display: flex; justify-content: center; align-items: center; gap: 20px; margin-bottom: 20px">
  <img src="https://www.erlang.org/favicon.ico" alt="Erlang Logo" style="width: 100px; height: auto;"/>
  <img src="https://www.docker.com/wp-content/uploads/2022/03/Moby-logo.png" alt="Docker Logo" style="width: 100px; height: auto;"/>
</div>

![Project Status](https://img.shields.io/badge/status-in%20development-yellow)
![License](https://img.shields.io/badge/license-Apache%202.0-blue)
![Version](https://img.shields.io/badge/version-0.1.0-blue)

A simple chat server built with Erlang and Docker.

## 📝 License

This project is licensed under the Apache License 2.0.

## 🎖️ Honorable Mentions

- [The Erlang Docs](https://www.erlang.org/)
- [Adopting Erlang](https://adoptingerlang.org/) by Tristan Sloughter, Fred
  Hebert, and Evan Vigil-McClanahan
- [The Cowboy Documentation](https://ninenines.eu/docs/en/cowboy/2.9/guide/) by
  Loïc Hoguin
- [Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/) by
  Fred Hebert
- ... and of course a myriad of other resources and contributors

## 🚀 Getting Started

### 📚 Prerequisites

- Erlang/OTP 24
- Rebar3
- Docker
- Docker Compose

### 🛠️ Development Setup

1. **Build the Docker image:**

   ```sh
   docker-compose down; docker-compose build
   ```

2. **Start the server** (with live code reloading):
   ```sh
   docker-compose up chat_server_development --watch
   ```

### 🧪 Running Tests

This project uses a combination of EUnit and Common Test for its testing
strategy. Tests are set up to run automatically on code changes, fitting the
Test Driven Development (TDD) lifecycle.

#### Setting up Automatic Test Running

1. **Install the file watcher tool `entr`:**

   On macOS with Homebrew:

   ```sh
   brew install entr
   ```

   For other systems, refer to the [entr installation
   guide](https://github.com/eradman/entr#installation).

2. **Run the automatic test watcher:**

   From the project root, run:

   ```sh
   ./watch_and_test.sh
   ```

   This will run both EUnit and Common Test whenever a `.erl` or `.hrl` file
   changes in the `src` or `test` directories.

3. **Stop the watcher:**

   Press `Ctrl + C` in the terminal where `watch_and_test.sh` is running.

The `run_tests.sh` and `watch_and_test.sh` scripts are already included in the
repository, so you don't need to create them manually.

When running the application directly with `rebar3` (e.g., using `rebar3 shell`,
`rebar3 eunit`, or `rebar3 ct`), the application uses a default port of 8081.
This differs from execution in release-based environments (development
containers, production, and CI), which use port 8080.

**Important Note for Contributors:**

- When executed directly with `rebar3`, the application will use port 8081 by
  default.
- This default port is hard-coded and not configurable through the usual
  configuration files when running directly with `rebar3`.
- If you need to change this port for non-release execution, you'll need to
  modify the `get_port/0` function in `src/chat_server.erl`.
- Remember that this port (8081) is specifically for non-release execution and
  does not affect release-based environments.

Example of the relevant code in `src/chat_server.erl`:

```erlang
get_port() ->
  application:get_env(chat_server, port, 8081).
```

Please be aware of this distinction when running the application directly with
`rebar3` versus in a release-based environment.

### 🏭 Production Setup

1. **Build the production Docker image:**

   ```sh
   docker-compose down; docker-compose -f docker-compose.prod.yml build
   ```

2. **Start the server in production mode:**
   ```sh
   docker-compose -f docker-compose.prod.yml up
   ```

## 🤝 Contributing

Contributions are welcome! If you find this project useful or have ideas for
improvements, please fork the repository and create a pull request.

## 📧 Contact

For any questions or inquiries, please contact [Zac Bolton](mailto:zacbolton2129@gmail.com).

---

### 📝 Notes

This project serves as a practical example of Erlang development and Docker
containerization. It aims to provide a solid foundation for further development
and potential commercial use. Feedback and suggestions are greatly appreciated.
