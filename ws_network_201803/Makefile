DOCKER_USER=mcooney
PROJECT_USER=kaybenleroll
PROJECT_NAME=ws_network_201803

#--user $(shell id -u):$(shell id -g)

docker-build-image: Dockerfile
	docker build -t ${PROJECT_USER}/${PROJECT_NAME} -f Dockerfile .

docker-run:
	docker run --rm \
	  -p 8787:8787 \
	  -v "${PWD}":"/home/${DOCKER_USER}/${PROJECT_NAME}":rw \
	  -e USER=${DOCKER_USER} \
	  -e PASSWORD=quickpass \
	  ${PROJECT_USER}/${PROJECT_NAME}

docker-stop:
	docker stop $(shell docker ps -q -a)

docker-clean:
	docker rm $(shell docker ps -q -a)

