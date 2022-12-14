#!/bin/bash

set -e

source /scripts/util.sh

if [ -f .seed ]; then
    echo "Using existing public DID."
    SEED=$(cat .seed)
else
    echo "Registering new public DID."
    SEED=$(
        tr -dc A-Za-z0-9 </dev/urandom | head -c 32
        echo ''
    )

    echo "$SEED" >.seed

    curl \
        -H "Content-Type: application/json" \
        -d '{"seed":"'"$SEED"'"}' \
        "$LEDGER_URL"/register
fi

TUNNEL_NAME="framework.$ORG" wait_for_ngrok
FRAMEWORK_ENDPOINT=$NGROK_ENDPOINT

NGROK_ENDPOINT=
TUNNEL_NAME="tails-server" NGROK_URL=ngrok-1:4041 wait_for_ngrok
TAILS_ENDPOINT=$NGROK_ENDPOINT

echo "Starting aca-py agent with endpoint [$FRAMEWORK_ENDPOINT]"

exec aca-py start \
    --endpoint "$FRAMEWORK_ENDPOINT" \
    --inbound-transport http '0.0.0.0' "$FRAMEWORK_PORT" \
    --outbound-transport http \
    --admin '0.0.0.0' "$ADMIN_PORT" \
    --admin-api-key "$ADMIN_SECRET" \
    --webhook-url "$WEBHOOK_URL" \
    --genesis-url "$LEDGER_URL"/genesis \
    --label "$AGENT_LABEL" \
    --wallet-type "askar" \
    --wallet-name "$WALLET_NAME" \
    --wallet-key "$WALLET_KEY" \
    --seed "$SEED" \
    --auto-provision \
    --auto-accept-requests \
    --notify-revocation \
    --monitor-revocation-notification \
    --tails-server-base-url "$TAILS_ENDPOINT"

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

# faber.agent

# "--auto-ping-connection",
# "--auto-respond-messages",
# "--preserve-exchange-records",
# "--public-invites",
# "--auto-accept-invites",
# "--auto-store-credential"
