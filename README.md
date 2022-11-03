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

1. Set environment variables in `.env` file. See instructions for each variable there.

2. Start DVLA network: `docker compose up`.

3. Connect Alice mobile device to DVLA network. Theoretically should work with different mobile agents, 
but was actually tested with Aries Bifold agent. Follow instructions for Aries Bifold setup in their [repo](https://github.com/hyperledger/aries-mobile-agent-react-native).
   - Use Indicio Testnet and Indicio public mediator

## Usage

1. Go to DVLA UI on `localhost:8002`. Generate Invitation and scan QR with Aries Bifold.

2. At this point you can send messages between Bifold and DVLA.  
   * In Bifold go to contacts, newest contact should be `DVLA Agent`. Send some message.

## TODOs:

1. Setup mediator

2. Connect to mediator with Aries Bifold
