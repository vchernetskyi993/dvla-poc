FROM node:19.1.0-slim 

ARG DIR=/app/
RUN mkdir $DIR
WORKDIR $DIR

# install dependencies
ADD package.json package-lock.json $DIR

RUN npm ci

# build front-end
ADD tsconfig.json tsconfig.json
ADD src src
ADD public public

RUN npm run build

# build server
ADD server.ts server.ts

RUN npx tsc server.ts

CMD [ "node", "server.js" ]
