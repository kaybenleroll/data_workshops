DOCKER_USER=mcooney

PROJECT_USER=kaybenleroll
PROJECT_NAME=ws_pgm_202009

render-html: ${PROJECT_NAME}.Rmd
	Rscript -e 'rmarkdown::render("${PROJECT_NAME}.Rmd")'


docker-build-image: Dockerfile
	docker build -t ${PROJECT_USER}/${PROJECT_NAME} -f Dockerfile .

docker-run:
	docker run --rm -d \
	  -p 8787:8787 \
	  -v "${PWD}":"/home/${DOCKER_USER}/${PROJECT_NAME}":rw \
	  -e USER=${DOCKER_USER} \
	  -e PASSWORD=quickpass \
		--name workshop \
	  ${PROJECT_USER}/${PROJECT_NAME}

docker-stop:
	docker stop $(shell docker ps -q -a)

docker-clean:
	docker rm $(shell docker ps -q -a)

