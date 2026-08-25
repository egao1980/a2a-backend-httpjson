# a2a-backend-httpjson

HTTP+JSON REST binding for [`a2a-protocol`](https://github.com/egao1980/a2a-protocol) (A2A 1.0).

| Method | Path |
|--------|------|
| GET | `/.well-known/agent-card.json` |
| POST | `/message:send` |
| POST | `/message:stream` (SSE) |
| GET | `/tasks`, `/tasks/{id}` |
| POST | `/tasks/{id}:cancel` |
| GET | `/extendedAgentCard` |

`A2A-Version` header is accepted; empty means `0.3`.

```lisp
(asdf:load-system "a2a-backend-httpjson")
(a2a-backend-httpjson:make-a2a-app
 (a2a-protocol:make-a2a-agent :name "echo"))
```

Part of [cl-stack](https://github.com/egao1980/cl-stack) agent-wire ([brief](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/agent-wire.md)).

CI: canned [`cl-repository`](https://github.com/egao1980/cl-repository) (`test-system.yml` / `setup-client` + `ci`). Deps from `ghcr.io/egao1980/cl-systems`.

## License

MIT
