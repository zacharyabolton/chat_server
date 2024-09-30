# [Adopting Erlang: Docker](https://adoptingerlang.org/docs/production/docker/)

The minimum Docker version required for this chapter is 19.03 with buildx
installed. Installing buildx can be done with the following commands:

```bash
$ export DOCKER_BUILDKIT=1
$ docker build --platform=local -o . git://github.com/docker/buildx
$ mv buildx ~/.docker/cli-plugins/docker-buildx
```

**This is included in Docker Desktop for Mac and Windows**

---

> There is a security concern to keep in mind when using layer caching. For
> example, since the RUN command only reruns if the text of the command changes,
> or a previous layer invalidated the cache, any system package installed will
> remain the same version even if a security fix has been released. For this
> reason it is good to occasionally run Docker with --no-cache which will not
> reuse any layers when building the image.

---

```bash
docker buildx build -o type=docker --target runner --tag chat_server:$(git rev-parse HEAD) .
```
