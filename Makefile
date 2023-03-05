all: start-server
PHONY := all build-builder build-release start-server develop

DOCKER := sudo docker
LOGIN_ENABLED := false

build-builder:
	$(DOCKER) build . -f .docker/Dockerfile -t rpt-builder

build-release: build-builder
	$(DOCKER) build . -f .docker/Dockerfile.runner -t rpt-runner

start-server: build-release
	$(DOCKER) run --rm -it -p 8080:80 -v "$(shell pwd)/exercises:/usr/local/apache2/cgi-bin/exercises" -e "RPT_LOGIN_ENABLED=$(LOGIN_ENABLED)" rpt-runner

develop: build-builder
	$(DOCKER) run --rm -it -v "$(shell pwd):/home/user/rpt" -v "/tmp:/tmp" -p 8080:80 -w /home/user/rpt rpt-builder
