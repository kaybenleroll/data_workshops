DOCKER_USER=mcooney
PROJECT_USER=kaybenleroll
PROJECT_NAME=talk_dds_bayesreg_201908


docker-build-image: Dockerfile
	docker build -t ${PROJECT_USER}/${PROJECT_NAME} -f Dockerfile .

docker-run:
	docker run --rm -d \
	  -p 8787:8787 \
	  -v "${PWD}":"/home/${DOCKER_USER}/${PROJECT_NAME}":rw \
	  -e USER=${DOCKER_USER} \
	  -e PASSWORD=quickpass \
	  ${PROJECT_USER}/${PROJECT_NAME}

docker-stop:
	docker stop $(shell docker ps -q -a)

docker-clean:
	docker rm $(shell docker ps -q -a)

