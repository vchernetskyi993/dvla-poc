from aiohttp import web
from sys import argv


async def create_invitation(_: web.Request):
    return web.json_response({"invitation_url": "http://my.awesome.url?code=qwerty"})


if __name__ == "__main__":
    app = web.Application()
    app.add_routes([web.post("/connections/create-invitation", create_invitation)])
    web.run_app(app, port=int((argv + [8080])[1]))
