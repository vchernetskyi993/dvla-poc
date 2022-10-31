FROM haskell:9.2.4 as build

RUN mkdir /project
WORKDIR /project

COPY app app
COPY src src
COPY stack.yaml package.yaml ./

RUN stack install

FROM alpine:3.16.2

RUN mkdir /app
WORKDIR /app
COPY .env .
COPY --from=build /root/.local/bin/dvla-exe .

ENTRYPOINT [ "./dvla-exe" ]
