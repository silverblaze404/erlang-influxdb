#!/usr/bin/env bash

echo "%% =========================== CT: Post Setup Phase [START] ================================  %%"
start_time=$(date +%s)
echo "%% ------------------------------------------------------------------------------- %%"
echo "STEP (1/2): Stopping postgres, zookeeper, kafka , influx and rabbitmq containers ........."
echo "%% ------------------------------------------------------------------------------- %%"

services=(
    "erlang_influxdb"
)
# Detect "docker compose" vs "docker-compose", prefer "docker compose" if available
if docker compose >/dev/null 2>&1; then
  COMPOSE_BIN="docker compose"
elif docker-compose >/dev/null 2>&1; then
  COMPOSE_BIN="docker-compose"
else
  echo "ERROR: docker compose or docker-compose not found"
  exit 1
fi
$COMPOSE_BIN -f ct-docker-compose.yml down "${services[@]}"

echo "%% ------------------------------------------------------------------------------- %%"
echo "STEP (2/2): Deleting Common Network "
echo "%% ------------------------------------------------------------------------------- %%"

echo "%% ------------------------------------------------------------------------------- %%"
echo "STEP (3/3): Deleting Volumes "
echo "%% ------------------------------------------------------------------------------- %%"
docker volume prune -f

echo "%% ------------------------------------------------------------------------------- %%"
echo "Docker PS"
echo "%% ------------------------------------------------------------------------------- %%"
docker ps -a

echo "%% ======================= CT: Post Setup Phase [END] ======================================  %%"

