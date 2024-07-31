# Chat Server

A simple chat server built with Erlang and Docker.

## License

This project is licensed under the Apache License 2.0.

## Getting Started

### Prerequisites

- Docker
- Docker Compose

### Development Setup

1. **Create a `.env` file** in the root of the project with the following
   content:

   ```plaintext
   SERVER_NODE=messenger@chat_server
   ```

2. **Build and start the Docker container:**

   ```sh
   docker-compose down && docker-compose build && docker-compose up
   ```

### Running Tests

Tests are set up to run automatically on code changes, fitting the Test Driven
Development (TDD) lifecycle.

1. **Start the test service:**

   ```sh
   docker-compose up chat_server_tests
   ```

This command will start a container that watches for file changes in the `src`
directory and runs the unit tests automatically using `rebar3 eunit`.

### Production Setup

1. **Create a `.env.prod` file** in the root of the project with the appropriate
   production configuration:

   ```plaintext
   SERVER_NODE=messenger@chat_server_prod
   ```

2. **Build the production Docker image:**

   ```sh
   docker-compose -f docker-compose.prod.yml build
   ```

3. **Start the server in production mode:**

   ```sh
   docker-compose -f docker-compose.prod.yml up
   ```

## Project Structure

- **src/**: Erlang source files.
- **Dockerfile**: Development Dockerfile.
- **Dockerfile.prod**: Production Dockerfile.
- **docker-compose.yml**: Docker Compose configuration for development.
- **docker-compose.prod.yml**: Docker Compose configuration for production.
- **.env**: Environment variables for development (ignored by Git).
- **.env.prod**: Environment variables for production (ignored by Git).

## Contributing

Contributions are welcome! If you find this project useful or have ideas for
improvements, please fork the repository and create a pull request.

### Setting Up Your Development Environment

1. **Fork the repository** and clone it to your local machine.
2. **Create a .env file** in the root directory with the necessary environment
   variables.
3. **Follow the development setup instructions** to build and start the
   Docker container.

## Contact

For any questions or inquiries, please contact [Zac Bolton](mailto:zacbolton2129@gmail.com).

---

### Notes

This project serves as a practical example of Erlang development and Docker
containerization. It aims to provide a solid foundation for further development
and potential commercial use. Feedback and suggestions are greatly appreciated.
