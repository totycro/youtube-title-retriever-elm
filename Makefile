all: reactor

RUN_IN_DOCKER = docker-compose run --user $$(id -u)

build:
	docker-compose build elm

compile: build
	${RUN_IN_DOCKER} elm make src/Main.elm

reactor: build
	# NOTE: elm is the container, not the command
	${RUN_IN_DOCKER} -p 8000:8000 elm reactor

test: build
	# TODO: ctrl-c doesn't seem to reach elm-test when launched this way
	${RUN_IN_DOCKER} --entrypoint elm-test elm --watch

bash: build
	${RUN_IN_DOCKER} --entrypoint bash elm
