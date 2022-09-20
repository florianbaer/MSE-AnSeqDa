.EXPORT_ALL_VARIABLES:

OS_NAME := $(shell uname -m)

ifeq ($(OS_NAME) , arm64)
ENV_FILE := .env.aarch
else
ENV_FILE := .env.latest
endif

# see https://stackoverflow.com/questions/4219255/how-do-you-get-the-list-of-targets-in-a-makefile
THIS_FILE := $(lastword $(MAKEFILE_LIST))

# Docker CMD:
DOCKER_CLI = docker compose run --entrypoint=$(ENTRYPOINT) -it notebook
DOCKER_START = docker compose --env-file ./$(ENV_FILE) up -d notebook && docker compose logs -f notebook
DOCKER_BUILD = docker compose --env-file ./$(ENV_FILE) build notebook 

# Include other definitions
include *.mk
###########################
# PROJECT UTILS
###########################
build: ##@Utils Builds the docker images
	$(DOCKER_BUILD)

###########################
# JUPYTER
###########################
start: ##@Jupyterlab Starts jupyterlab service
	@echo "Starting jupyter lab"
	$(DOCKER_START)

stop: ##@Jupyterlab Stops all services including the database
	@echo "Stop Jupyter lab, Mongo DB and MongoExpress..."
	docker compose down -v
