VERSION = $(shell cat DESCRIPTION | grep Version | sed 's/Version: //')
R       = R -q -e

test:
	$(R) "devtools::test()"

check:
	$(R) "devtools::check()"

build_src:
	$(R)"devtools::build()"

build_doc:
	$(R) "pkgdown::build_site()"

document:
	$(R) "devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))"

install: document
	$(R) "devtools::install(upgrade_dependencies = F)"

auth:
	docker login -u ${DOCKER_HUB_USERNAME} -p ${DOCKER_HUB_PASSWORD}

build:
	docker build -t nextcode/gor-r:${VERSION} .
	docker tag nextcode/gor-r:${VERSION} nextcode/gor-r:latest

build_notebook:
	docker build -f Dockerfile.rnotebook -t nextcode/gor-r-notebook:${VERSION} .

push:
	docker push nextcode/gor-r:${VERSION}
	docker push nextcode/gor-r:latest

run:
	docker run --rm --name gor-r -p 80:8787 -t nextcode/gor-r:${VERSION}

tag:
	git tag ${VERSION}
	git push origin --tags
