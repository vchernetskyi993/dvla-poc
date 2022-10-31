# Driver and Vehicle Licensing Agency DIDs

Personal learning POC project to practice DIDs and Hyperledger Aries. No production usage intended.

## Overview

<!-- TODO: add diagram -->

Players:

1. DVLA

   - issues and revokes driver licenses

2. Alice

   - receives and hold her driver license

3. Pub

   - verifies Alice age using driver license

## Local setup

1. Start [von-network](https://github.com/bcgov/von-network/blob/main/docs/UsingVONNetwork.md) using their instructions.

2. Set environment variables in `.env` file. See instructions for each variable there.

3. Start DVLA network: `docker compose up`.

4. Connect Alice mobile device to DVLA network:

```shell
curl localhost:4040/api/tunnels | jq '.tunnels | map({service: .name, url: .public_url})'
```

<!-- TODO: Alice steps -->

## Usage

<!-- TODO: describe happy pass flow -->

## TODOs:

1. Setup mediator

2. Connect to mediator with Aries Bifold
