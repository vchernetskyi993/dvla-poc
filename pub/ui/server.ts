import * as express from "express";
import * as dotenv from "dotenv";
import { createProxyMiddleware } from "http-proxy-middleware";

dotenv.config();
const app = express();

app.use(
  "/api",
  createProxyMiddleware({
    target: process.env.CONTROLLER_URL,
  })
);

app.use(express.static('build'))

const port = process.env.SERVER_PORT;
app.listen(port, () => console.log(`Express listening on ${port}`));
