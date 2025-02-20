# Chat Server

![CI Status](https://github.com/zacharyabolton/chat_server/workflows/chat_server%20CI/badge.svg)
![Erlang Version](https://img.shields.io/badge/Erlang-OTP%2026-blue)
![Rebar3 Version](https://img.shields.io/badge/Rebar3-3.23.0-blue)
![Node.js Version](https://img.shields.io/badge/Node.js-22.9.0-blue)
![Project Status](https://img.shields.io/badge/status-in%20development-yellow)
![License](https://img.shields.io/badge/license-Apache%202.0-blue)
![Version](https://img.shields.io/badge/version-0.1.0-blue)

<div align="center">
  <img src="https://www.erlang.org/favicon.ico" alt="Erlang Logo" width="80" />
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <img src="https://upload.wikimedia.org/wikipedia/commons/a/a7/React-icon.svg" alt="React Logo" width="80" />
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
</div>

A simple chat application built with **Erlang** (backend) and **React with TypeScript** (frontend), containerized using **Docker**.

---

## 📋 Table of Contents

- [Features](#features)
- [License](#license)
- [Honorable Mentions](#honorable-mentions)
- [Getting Started](#getting-started)
  - [Requirements](#requirements)
  - [Development Setup](#development-setup)
    - [Running the Backend](#running-the-backend)
    - [Running the Frontend](#running-the-frontend)
  - [Running Tests](#running-tests)
    - [Backend Tests](#backend-tests)
    - [Frontend Tests](#frontend-tests)
  - [Production Setup](#production-setup)
- [Contributing](#contributing)
- [Contact](#contact)
- [Notes](#notes)

---

## 🌟 Features

- **Real-time Communication:** WebSocket-based chat functionality.
- **Scalable Architecture:** Backend built with Erlang/OTP for high concurrency and fault tolerance.
- **Modern Frontend:** Responsive UI built with React and TypeScript.
- **Containerization:** Easy deployment using Docker and Docker Compose.
- **Continuous Integration:** Automated testing and building with GitHub Actions.

---

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

### 📚 Requirements

- **Erlang/OTP 26 Erts 14.2.5**
- **rebar 3.23.0**
- **Node.js 18.x**
- **Docker**
- **Docker Compose**

### 🛠️ Development Setup

#### **Project Structure**

```
.
├── client/ # React frontend
└── server/ # Erlang backend
```

#### **Running the Backend**

1. **Navigate to the `server` directory:**

   ```sh
   cd server
   ```

2. **Install dependencies:**

   ```sh
   rebar3 get-deps
   ```

3. **Compile the backend:**

   ```sh
   rebar3 compile
   ```

4. **Run the backend server:**

   ```sh
   rebar3 shell
   ```

   - The backend runs on port 8080 by default.

#### **Running the Frontend**

1. **Navigate to the `client` directory:**

   ```sh
   cd client
   ```

2. **Install dependencies:**

   ```sh
   npm install
   ```

3. **Start the development server:**

   ```sh
   npm start
   ```

   - The frontend runs on port 3000 by default.
   - The app should automatically open in your default browser.
   - The frontend is configured to proxy API requests to the backend.

#### **Using Docker Compose (Alternative)**

_NOTE_: Hot reloading is much slower when using Docker Compose compared to
running the frontend and backend separately.

For an integrated development environment, you can use Docker Compose to run
both the frontend and backend:

1. **Build the Docker image:**

   ```sh
   docker-compose down; docker-compose build
   ```

2. **Start the server** (with live code reloading):

   ```sh
   docker-compose up chat_server_development --watch
   ```

3. **Access the application:**
   - Visit [http://localhost:8080](http://localhost:8080) to access the frontend
     served by the backend.

### 🧪 Running Tests

#### **Backend Tests**

1. **Navigate to the `server` directory:**

   ```sh
   cd server
   ```

2. **Run EUnit and Common Test suites:**

   ```sh
   rebar3 do eunit, ct
   ```

3. **Run Dialyzer for static analysis:**

   ```sh
   rebar3 dialyzer
   ```

#### **Frontend Tests**

1. **Navigate to the `client` directory:**

   ```sh
   cd client
   ```

2. **Run tests:**

   ```sh
   npm test
   ```

   - Press `a` to run all tests.

3. **Run ESLint for linting:**

   ```sh
   npm run lint
   ```

---

### 🏭 Production Setup

#### **Build the Production Docker Image**

1. **Build the Docker image:**

   ```sh
   docker-compose -f docker-compose.prod.yml build
   ```

2. **Start the server in production mode:**

   ```sh
   docker-compose -f docker-compose.prod.yml up
   ```

3. **Access the application:**

   - Visit `http://localhost:8080` to access the application.

---

## 🤝 Contributing

Contributions are welcome! Here's how you can help:

1. **Fork the repository.**

2. **Create a new branch:**

   ```sh
   git checkout -b feature/YourFeature
   ```

3. **Make your changes and commit them:**

   ```sh
   git commit -m 'Add some feature'
   ```

4. **Push to the branch:**

   ```sh
   git push origin feature/YourFeature
   ```

5. **Open a pull request.**

---

## 📧 Contact

For any questions or inquiries, please contact [Zac
Bolton](mailto:zacbolton2129@gmail.com).

---

## 📝 Notes

This project serves as a practical example of Erlang and React development with
Docker containerization. It aims to provide a solid foundation for further
development and potential commercial use. Feedback and suggestions are greatly
appreciated.

---

## 📄 Additional Resources

- **CI/CD Pipeline:**

  - The project uses GitHub Actions for continuous integration (CI).
  - The pipeline includes testing, and building.
  - See the [`.github/workflows/ci.yml`](.github/workflows/ci.yml) file for details.

- **Caching and Optimization:**

  - Dependencies are cached in the CI pipeline to speed up builds.
  - Frontend and backend jobs run in parallel to reduce build times.

---

## 📦 Dependencies

- **Backend:**

  - Erlang/OTP 26
  - rebar3

- **Frontend:**

  - React
  - TypeScript
  - Node.js 22.9.0
  - npm

---

## ❤️ Acknowledgments

Special thanks to all the contributors and the open-source community for their
invaluable resources and support. See the [Honorable
Mentions](#honorable-mentions) section for more details.
