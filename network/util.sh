#!/bin/sh

#######################################
# Wait for Ngrok endpoint with TUNNEL_NAME.
# Globals:
#   TUNNEL_NAME
# Outputs:
#   NGROK_ENDPOINT
#######################################
wait_for_ngrok() {
    while [ -z "$NGROK_ENDPOINT" ]; do
        echo "Fetching end point from ngrok service"
        NGROK_ENDPOINT=$(curl ngrok:4040/api/tunnels |
            jq --arg name "$TUNNEL_NAME" -r '.tunnels[] | select(.name==$name) | .public_url')

        if [ -z "$NGROK_ENDPOINT" ]; then
            echo "ngrok not ready, sleeping 5 seconds...."
            sleep 5
        fi
    done
}
