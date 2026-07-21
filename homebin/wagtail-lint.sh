#!/bin/sh

docker compose exec web black $1
docker compose exec web isort $1
docker compose exec web flake8 $1
