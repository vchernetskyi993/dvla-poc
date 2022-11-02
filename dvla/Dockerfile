# TODO: use alpine for build
FROM haskell:9.2.4 as build

RUN mkdir /project
WORKDIR /project

COPY stack.yaml package.yaml ./
RUN stack build --dependencies-only

COPY app app
COPY src src
RUN stack install

FROM alpine:3.16.2

RUN apk update && apk add libc6-compat gmp

RUN mkdir /app
WORKDIR /app
COPY .env .
COPY --from=build /root/.local/bin/dvla-exe .

CMD [ "./dvla-exe" ]
