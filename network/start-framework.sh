#!/bin/bash

set -e

while [ -z "$NGROK_ENDPOINT" ]; do
    echo "Fetching end point from ngrok service"
    NGROK_ENDPOINT=$(curl ngrok:4040/api/tunnels | jq -r '.tunnels[] | select(.name=="framework.dvla") | .public_url')

    if [ -z "$NGROK_ENDPOINT" ]; then
        echo "ngrok not ready, sleeping 5 seconds...."
        sleep 5
    fi
done

echo "Starting aca-py agent with endpoint [$NGROK_ENDPOINT]"

exec aca-py start \
    --endpoint "$NGROK_ENDPOINT" \
    --inbound-transport http '0.0.0.0' "$FRAMEWORK_PORT" \
    --outbound-transport http \
    --admin '0.0.0.0' "$ADMIN_PORT" \
    --admin-api-key "$ADMIN_SECRET" \
    --webhook-url http://"$CONTROLLER_HOST":"$CONTROLLER_PORT"/api/webhooks \
    --genesis-url "https://raw.githubusercontent.com/Indicio-tech/indicio-network/main/genesis_files/pool_transactions_testnet_genesis" \
    --label "$AGENT_LABEL" \
    --wallet-type "askar" \
    --wallet-name "$WALLET_NAME" \
    --wallet-key "$WALLET_KEY" \
    --auto-provision

# agent-docker

# --auto-ping-connection \
# --monitor-ping \
# --public-invites \
# --wallet-type "indy" \
# --wallet-name "test_author" \
# --wallet-key "secret_key" \
# --wallet-storage-type "postgres_storage" \
# --wallet-storage-config "{\"url\":\"wallet-db:5432\",\"max_connections\":5}" \
# --wallet-storage-creds "{\"account\":\"DB_USER\",\"password\":\"DB_PASSWORD\",\"admin_account\":\"postgres\",\"admin_password\":\"mysecretpassword\"}" \
# --endorser-protocol-role author \
# --endorser-alias 'Endorser' \
# --auto-request-endorsement \
# --auto-write-transactions \
# --auto-create-revocation-transactions \

# alice-kt

# "--auto-accept-invites",
# "--auto-store-credential"

# faber.agent

# "--auto-ping-connection",
# "--auto-respond-messages",
# "--preserve-exchange-records",
# "--auto-provision",
# "--public-invites",
# "--genesis-transactions",
# "--seed",
# "d_000000000000000000000000359722",
# "--notify-revocation",
# "--monitor-revocation-notification",
# "--tails-server-base-url",
# "http://4f3e-156-146-50-1.ngrok.io",
# "--auto-accept-invites",
# "--auto-accept-requests",
# "--auto-store-credential"
