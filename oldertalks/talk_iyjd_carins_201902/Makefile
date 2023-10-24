DOCKER_USER=mcooney

PROJECT_USER=kaybenleroll
PROJECT_NAME=talk_iyjd_carins_201902

IMAGE_TAG=${PROJECT_USER}/${PROJECT_NAME}:latest


render-html: ${PROJECT_NAME}.Rmd
	Rscript -e 'rmarkdown::render("${PROJECT_NAME}.Rmd")'


docker-build-image: Dockerfile
	docker build -t ${IMAGE_TAG} -f Dockerfile .

docker-run:
	docker run --rm -d \
	  -p 8787:8787 \
	  -v "${PWD}":"/home/${DOCKER_USER}/${PROJECT_NAME}":rw \
	  -e USER=${DOCKER_USER} \
	  -e PASSWORD=quickpass \
	  ${IMAGE_TAG}

docker-stop:
	docker stop $(shell docker ps -q -a)

docker-clean:
	docker rm $(shell docker ps -q -a)

docker-pull:
	docker pull ${IMAGE_TAG}

docker-push:
	docker push ${IMAGE_TAG}