# Driver and Vehicle Licensing Agency DIDs

Personal learning POC project to practice DIDs and Hyperledger Aries. No production usage intended.

## Overview

<!-- TODO: add diagram -->

### Players:

1. DVLA

   - issues and revokes driver licenses

2. Alice

   - receives and hold her driver license

3. Pub

   - verifies Alice age using driver license

### Repo structure:

1. [dvla](./dvla/) - DVLA organization controller
2. [framework-stub](./framework-stub/) - stub for ACA-Py (used for local testing)
3. [network](./network/) - files for local network
4. [pub](./pub) - Pub organization controller

## Local setup

1. Set environment variables in `.env` file. See instructions for each variable there.

2. Start DVLA network: `docker compose up`.

3. Connect Alice mobile device to DVLA network. Theoretically should work with different mobile agents,
   but was actually tested with Aries Bifold agent. Follow instructions for Aries Bifold setup in their [repo](https://github.com/hyperledger/aries-mobile-agent-react-native).
   - Set `MEDIATOR_URL` to Indicio public mediator invitation from [here](https://indicio-tech.github.io/mediator/).
   - Set `GENESIS_URL` to BCovrin Test (http://test.bcovrin.vonx.io/genesis)

## Usage

1. Go to DVLA UI on `localhost:8002`. Generate Invitation and scan QR with Aries Bifold.

2. At this point you can send messages between Bifold and DVLA.

   - Bifold -> DVLA: In Bifold go to contacts, newest contact should be `DVLA Agent`.
     Send some message. You should see message in controller logs.
   - DVLA -> Bifold: Issue `curl -i -X POST -H "Content-Type: application/json" -d '{"connectionId": "e291815e-c58d-4d36-90ed-6a6cc63c23ca", "text": "Hello!"}' localhost:8002/api/messages` to send "Hello!" message to mobile app.
     - To find `connectionId` either check controller logs or issue `curl localhost:8002/api/connections`.

3. Create schema and credential definition for driver license: `curl -i -X POST localhost:8002/api/schemas`.

4. Issue driver license to mobile agent (don't forget to set correct `connectionId`):
   `curl -i -X POST -H "Content-Type: application/json" -d '{"connectionId": "e291815e-c58d-4d36-90ed-6a6cc63c23ca", "attributes": {"firstName": "Alice", "lastName": "Doe", "category":"B1", "dateOfBirth": "19891109"}}' localhost:8002/api/licenses`.
   You should see credential offer in Bifold. Accept it.

5. Go to Pub UI at `localhost:8013`. Generate proof request and scan it with Bifold.
   You should be able to prove your name and age using fresh driver license.

TODO:

- add revocation
