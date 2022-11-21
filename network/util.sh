#!/bin/sh

#######################################
# Wait for Ngrok endpoint with TUNNEL_NAME.
# Globals:
#   TUNNEL_NAME
#   NGROK_URL=ngrok:4040
# Outputs:
#   NGROK_ENDPOINT
#######################################
wait_for_ngrok() {
    NGROK_URL=${NGROK_URL:-ngrok:4040}
    while [ -z "$NGROK_ENDPOINT" ]; do
        echo "Fetching end point from ngrok service"
        NGROK_ENDPOINT=$(curl "$NGROK_URL"/api/tunnels |
            jq --arg name "$TUNNEL_NAME" -r '.tunnels[] | select(.name==$name) | .public_url')

        if [ -z "$NGROK_ENDPOINT" ]; then
            echo "ngrok not ready, sleeping 5 seconds...."
            sleep 5
        fi
    done
}
