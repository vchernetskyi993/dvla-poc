# ACA-Py Stub

Created to provide stubbed framework with minimal subset of endpoints for local testing.

## Setup

1. Install `requirements.txt` as you like.

2. Start stub:
```shell
# start on 8080 port
python main.py

# start on custom port
python main.py 8000
```

## Usage

Used this stub for performance testing using [wrk](https://github.com/wg/wrk).

1. Start [dvla](../dvla)

2. Start stub on port 8001: `python main.py 8001`

3. Run wrk: `wrk -t4 -c400 -d30s -s wrk/post.lua http://127.0.0.1:8080/api/invitations`
