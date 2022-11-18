#!/bin/sh

set -e

. /scripts/util.sh

TUNNEL_NAME="$TUNNEL_NAME" wait_for_ngrok

echo "Starting service with endpoint [$NGROK_ENDPOINT]"

PUBLIC_URL=$NGROK_ENDPOINT/api
export PUBLIC_URL

exec $1
