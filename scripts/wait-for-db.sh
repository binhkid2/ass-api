#!/bin/bash
# wait-for-db.sh: Wait for database to be ready

set -e

host="$1"
port="$2"
shift 2
cmd="$@"

until pg_isready -h "$host" -p "$port"; do
  echo "Database is unavailable - sleeping"
  sleep 1
done

echo "Database is up - executing command"
exec $cmd
