FROM gradle:7.5.1-jdk17-alpine as build

ARG APP_DIR=/app/
RUN mkdir $APP_DIR
WORKDIR $APP_DIR

ADD build.gradle.kts settings.gradle.kts gradle.properties $APP_DIR
ADD src/main src/main

RUN gradle installDist

FROM amazoncorretto:17.0.5-alpine3.16

COPY --from=build /app/build/install/pub /app

ENTRYPOINT ["/app/bin/pub"]
