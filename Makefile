APP_NAME=positive-imp
PLATFORM=linux/arm64

build:
	docker build --platform=$(PLATFORM) -t $(APP_NAME) .

run:
	docker run -it --rm $(APP_NAME)

start: build run
