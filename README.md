# Chat Server

<mark>THE FOLLOWING README IS DEFUNCT AND WILL BE UPDATED SHORTLY!</mark>

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

## 🚀 Getting Started

### Prerequisites

- Docker
- Docker Compose

### Development Setup

1. **Create a `rebar.config.local` file** in the root of the project with the
   following content:

   ```erlang
   {dist_node, [
       {setcookie, 'your_local_cookie'},
       {sname, 'your_local_node_name'}
   ]}.
   ```

   Note: Make sure to add `rebar.config.local` to your `.gitignore` file.

2. **Build and start the Docker container:**

   ```sh
   docker-compose down; docker-compose build
   ```

3. **Start the server:**
   ```sh
   docker-compose up chat_server
   ```

### 🧪 Running Tests

Tests are set up to run automatically on code changes, fitting the Test Driven
Development (TDD) lifecycle.

1. **Start the test service:**
   ```sh
   docker-compose up chat_server_tests
   ```

This command will start a container that watches for file changes in the `src`
directory and runs the unit tests automatically using `rebar3 eunit`.

### 🏭 Production Setup

1. **Build the production Docker image:**

   ```sh
   docker-compose -f docker-compose.prod.yml build
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
- **rebar.config.script**: Script to include local configuration.
- **rebar.config.local**: Local configuration file (not version controlled).

## 🤝 Contributing

Contributions are welcome! If you find this project useful or have ideas for
improvements, please fork the repository and create a pull request.

### Setting Up Your Development Environment

1. **Fork the repository** and clone it to your local machine.
2. **Create a `rebar.config.local` file** in the root directory with the
   necessary local settings.
3. **Follow the development setup instructions** to build and start the
   Docker container.

## 📧 Contact

For any questions or inquiries, please contact [Zac Bolton](mailto:zacbolton2129@gmail.com).

---

### 📝 Notes

This project serves as a practical example of Erlang development and Docker
containerization. It aims to provide a solid foundation for further development
and potential commercial use. Feedback and suggestions are greatly appreciated.
