FROM node:18.12.0-slim as ui-build

COPY ui/package.json ui/package-lock.json ui/tsconfig.json /ui/
COPY ui/public /ui/public
COPY ui/src /ui/src
WORKDIR /ui

RUN npm ci

RUN npm run build

FROM haskell:9.2.4 as server-build

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
COPY --from=server-build /root/.local/bin/dvla-exe .
COPY --from=ui-build /ui/build ./ui/build

CMD [ "./dvla-exe" ]
