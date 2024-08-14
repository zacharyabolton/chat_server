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

Tests are set up to run automatically on code changes, fitting the Test Driven
Development (TDD) lifecycle.

1. **Install `rebar3_autotest` globally:**

   - [github.com/NobbZ/rebar3_autotest](https://github.com/NobbZ/rebar3_autotest)

2. **Run the autotest command** in your project's root:
   ```sh
   rebar3 autotest
   ```

This command will run all unit tests in the `./test` directory whenever a file
changes in the `./src` directory.

### 🏭 Production Setup

1. **Build the production Docker image:**

   ```sh
   docker-compose down; docker-compose -f docker-compose.prod.yml build
   ```

2. **Start the server in production mode:**
   ```sh
   docker-compose -f docker-compose.prod.yml up
   ```

## 📂 Project Structure

- **src/**: Erlang source files.
- **Dockerfile**: Development Dockerfile.
- **Dockerfile.prod**: Production Dockerfile.
- **docker-compose.yml**: Docker Compose configuration for development.
- **docker-compose.prod.yml**: Docker Compose configuration for production.
- **rebar.config**: Main rebar3 configuration file.

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
