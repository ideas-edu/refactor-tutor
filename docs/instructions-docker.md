# Installation Instructions Docker
If you want to get started using docker, make sure you have docker installed first.

You can use the `stephanstanisic/refactor-tutor` image on docker hub to deploy the refactor tutor together with a preconfigured apache2 webserver.
To do so, you will first need to create a folder with exercises. These have the following structure:

```
exercises/
  name-of-ex1/
    start.java
    description.txt
    tests.txt
```

Every exercise needs a start situation, a textual description and a set of testcases that show correct behaviour. Folders that start with a period `.` are ignored. Examples of these can be found in the repository under the exercises folder.
Once you have a set of exercises you can use the following docker command to start a container:

```bash
docker run \
  -p 8080:80 \
  -v "$(pwd)/exercises:/usr/local/apache2/cgi-bin/exercises" \
  -e "RPT_LOGIN_ENABLED=false" \
  stephanstanisic/refactor-tutor
```

Alternatively, you can use the following `docker-compose.yml` file:

```yaml
version: '3'

services:
  refactor-tutor:
    image: stephanstanisic/refactor-tutor
    ports:
    - "8080:80"
    volumes:
    - "./exercises:/usr/local/apache2/cgi-bin/exercises"
    environment:
    - "RPT_LOGIN_ENABLED=false"
```

Then use `docker compose up -d` to start the container.

After using either of these two methods the refactor tutor will be available at `http://localhost:8080`. If you want to expose this to the internet, you will have to configure a TLS-terminating reverse proxy yourself.


