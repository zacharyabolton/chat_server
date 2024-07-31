# Chat Server

A simple chat server built with Erlang and Docker.

## License

This project is licensed under the Apache License 2.0.

## Getting Started

### Prerequisites

- Docker
- Docker Compose

### Development Setup

1. **Build and start the Docker container:**

   ```sh
   docker-compose down && docker-compose build && docker-compose up
   ```

### Production Setup

1. **Build the production Docker image:**

   ```sh
   docker-compose -f docker-compose.prod.yml build
   ```

2. **Start the server in production mode:**

   ```sh
   docker-compose -f docker-compose.prod.yml up
   ```

## Project Structure

- **src/**: Erlang source files.
- **Dockerfile**: Development Dockerfile.
- **Dockerfile.prod**: Production Dockerfile.
- **docker-compose.yml**: Docker Compose configuration for development.
- **docker-compose.prod.yml**: Docker Compose configuration for production.

## Contributing

Contributions are welcome! If you find this project useful or have ideas for
improvements, please fork the repository and create a pull request.

## Contact

For any questions or inquiries, please contact [Zac Bolton](mailto:zacbolton2129@gmail.com).

---

### Notes

This project serves as a practical example of Erlang development and Docker
containerization. It aims to provide a solid foundation for further development
and potential commercial use. Feedback and suggestions are greatly appreciated.
