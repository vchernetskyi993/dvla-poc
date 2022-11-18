# Pub Application

Sample application for requesting age proof from customer.

Uses [Camel Aries](https://camel.apache.org/components/next/hyperledger-aries-component.html) integration.

## Usage

Until Aries component in Camel is updated project expects related libraries in `libs` folder.
To correctly populate it:

1. Install SNAPSHOT version of aries component: `cd <camel aries dir>; mvn install`.
2. Copy everything camel-related from `.m2` to `libs`: `cp -rf ~/.m2/repository/org/apache/camel/* libs/org/apache/camel/`.

Build and run: `./gradlew clean run`.
