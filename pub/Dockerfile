FROM gradle:7.5.1-jdk17-alpine as build

ARG APP_DIR=/app/
RUN mkdir $APP_DIR
WORKDIR $APP_DIR

ADD build.gradle.kts settings.gradle.kts gradle.properties $APP_DIR
ADD src/main src/main

# FIXME: remove when aries component is published
ADD libs libs

RUN gradle installDist

FROM amazoncorretto:17.0.5-alpine3.16

RUN apk add curl jq

COPY --from=build /app/build/install/pub /app

ENTRYPOINT ["/app/bin/pub"]
