#!/bin/sh

docker compose exec web npm outdated
docker compose exec web npm update
docker compose exec web npm run watch

