# AI DSL — Fully Composable Building Blocks for AI Conversations

> **Everything is composable from the ground up.**  
> This is not a framework. It is a set of zero-coupling building blocks — network transport, platform adapters, tool system, conversation environment, and terminal styling — each with a unified interface, ready to be composed into any form of AI interaction.

**License:** MIT

---

## One Minute to Start

### Raw blocks (max flexibility):

```racket
#lang racket
(require "api-platform/deepseek/chat.rkt"
         "api-platform/deepseek/json-build-parse.rkt"
         "api-config/deepseek.rkt")

(define req (build-chat-request
             (hasheq 'model deepseek-v4-flash
                     'messages (build-messages
                                (build-user-message #:content "Introduce Racket."))
                     'max_tokens 256)))
(define resp (deepseek-chat req))
(display (response-content resp))
```

### With env (less boilerplate):

```racket
#lang racket
(require "env/deepseek-env.rkt"
         "api-config/deepseek.rkt")

(define my-env (make-env #:model deepseek-v4-flash #:max_tokens 256))
(display (response-content (env-chat my-env "Introduce Racket.")))
```

Run it:

```bash
export DEEPSEEK_API_KEY="sk-..."
racket my-first-chat.rkt
```

---

## Building Blocks at a Glance

```
-──────────────────────────────────────────────
 test-deepseek.rkt  test-tongyi.rkt
 env/deepseek-env.rkt  env/tongyi-env.rkt  ← NEW: Environment abstraction
-──────────────────────────────────────────────
                                              
  Core Building Blocks (freely composable)     
                                              
  ┌──────────┐ ┌──────────┐ ┌──────────┐     
  │  Tools   │ │ History  │ │ Terminal │     
  │  system  │ │ manager  │ │ styling  │     
  │ tools/   │ │ history/ │ │ format-  │     
  │          │ │          │ │ color/   │     
  └──────────┘ └──────────┘ └──────────┘     
                                              
  ┌──────────────────────────────────┐        
  │ Platform Adapters (pluggable)    │        
  │ api-platform/deepseek/           │        
  │ api-platform/tongyi/             │        
  │ — same interface, interchangeable │        
  └──────────────────────────────────┘        
                                              
  ┌──────────────────────────────────┐        
  │ Network Transport net-io.rkt     │        
  │ pure HTTP + SSE, no platform     │        
  │ logic                            │        
  └──────────────────────────────────┘        
                                              
  ┌──────────────────────────────────┐        
  │ API Config api-config/           │        
  │ URLs, model names, API keys      │        
  └──────────────────────────────────┘        
-──────────────────────────────────────────────
```

Every layer is an independent Racket module. Zero coupling, zero implicit dependencies.

---

## Environment Abstraction (env/)

The `env/` layer sits above the platform adapters and reduces boilerplate by bundling
configuration (model, parameters, tools, callbacks) into a reusable **environment**.

### Two files, one pattern

| File | Platform | Extra options |
|------|----------|---------------|
| `env/deepseek-env.rkt` | DeepSeek | `thinking`, `reasoning-effort` |
| `env/tongyi-env.rkt` | Tongyi/Qwen | — |

Both export the **same interface**:

| Function | Purpose |
|----------|---------|
| `make-env` | Create environment (all options optional) |
| `env-set` | Create a modified copy (per-call overrides) |
| `env-chat` | Non-streaming chat: `messages → response` |
| `env-chat/stream` | Streaming chat with callbacks |
| `tool-chat-request` | Execute tools from response, return new messages |
| `tool-stop-chat-request` | Same + returns `(values has-tools? messages)` |
| `env-print` | Display environment info |

And re-exports response parsing functions:
`response-content`, `response-model`, `response-tool-calls`, `tool-call-func-name`, etc.

### Why env?

**Before** (raw blocks, ~8 lines per call):

```racket
(define messages (build-messages (build-user-message #:content "Hi")))
(define req (build-chat-request (hasheq 'model deepseek-v4-flash
                                        'messages messages
                                        'max_tokens 256)))
(define resp (deepseek-chat req))
```

**After** (env, 1 line):

```racket
(define resp (env-chat my-env "Hi"))
```

### Typical usage

```racket
(require "env/deepseek-env.rkt"
         "api-config/deepseek.rkt"
         "tools/deepseek-base-tool.rkt")

(define my-env
  (make-env #:model deepseek-v4-flash
            #:max_tokens 4096
            #:tools default-tools
            #:on-content (lambda (c) (display c) (flush-output))
            #:on-reasoning (lambda (r) (display r))))

;; Non-streaming
(env-chat my-env "Yong yi ju hua jie shao ni zi ji.")

;; Streaming (callbacks from env)
(env-chat/stream my-env "Cong 1 shu dao 5")

;; With thinking mode
(define thinking-env
  (env-set my-env #:thinking #t #:reasoning-effort "high"))
(env-chat/stream thinking-env "9.9 he 9.11 na ge da?")

;; Composable tool loop (no loop built-in)
(let loop ([msgs (build-messages (build-user-message #:content "yun xing whoami"))]
           [n 0])
  (define resp (env-chat my-env msgs))
  (let-values ([(more? msgs2) (tool-stop-chat-request default-tools resp msgs)])
    (if more? (loop msgs2 (add1 n)) (display "done\n"))))
```

---

## More Composition Examples

### Streaming Chat

```racket
(env-chat/stream my-env "Tell me a story")
```

### With per-call overrides

```racket
(env-chat/stream my-env "Hello"
  #:on-content (lambda (c) (printf ">> ~a" c))
  #:handlers (list (cons 'reasoning (lambda (r) (void)))))
```

### Tool Loop (composable, no built-in loop)

```racket
;; tool-chat-request: pure function, one-shot
(define msgs2 (tool-chat-request default-tools resp msgs))

;; tool-stop-chat-request: with stop detection
(let-values ([(more? msgs2) (tool-stop-chat-request default-tools resp msgs)])
  (if more? (loop msgs2) (display "done\n")))
```

### Switch Platform

```racket
(require "env/tongyi-env.rkt" "api-config/tongyi.rkt")
(define ty-env (make-env #:model tongyi-qwen-max #:max_tokens 4096))
(env-chat ty-env "Hello")  ;; same interface
```

---

## Design Decisions

### Environment is not a framework

The `env/` layer is **optional**. You can always drop down to raw blocks
(`deepseek-chat`, `build-chat-request`, etc.) for full control.
The env is just a bundling convenience — it owns no state, has no hidden loops.

### Composable tool primitives

`tool-chat-request` and `tool-stop-chat-request` are **pure functions**:
- They do NOT call the API
- They do NOT loop
- They just transform `response + messages → new-messages`
- You compose them with standard Racket (`let loop`, `for/fold`, etc.)

### Hash Table Parameters (Runtime Parameter Discovery)

`build-chat-request` and `build-message` accept a **single hash table** instead of keyword arguments:

```racket
(build-chat-request (hasheq 'model m 'messages msgs 'stream #t 'max_tokens 256))
```

**Why?** Keyword arguments with defaults hide what parameters are available.
A hash table is self-describing and introspectable:

```racket
chat-request-keys  ;; => '(model messages stream max_tokens temperature top_p ...)
message-keys       ;; => '(role content reasoning_content tool_calls tool_call_id name)
```

### `#:stop?` Default in Stream Functions

`deepseek-chat/stream` and `env-chat/stream` accept an optional `#:stop?` predicate:

```racket
(env-chat/stream my-env "translate this"
  #:stop? (lambda () (> (string-length buf) 1000)))
```

Default is `(lambda () #f)` — never stop.

---

## Learning Path

| Step | File | Content |
|------|------|---------|
| 1 | `test-deepseek.rkt` examples 1–2 | Non-streaming / streaming with env |
| 2 | `test-deepseek.rkt` example 3 | Streaming + thinking mode |
| 3 | `test-deepseek.rkt` example 4 | Non-streaming + tool calls |
| 4 | `test-deepseek.rkt` example 5 | Streaming + tool loop (composable) |
| 5 | `test-tongyi.rkt` | Same patterns on Tongyi platform |

---

## Requirements

- **Racket** (recommended v8.6+, minimum 64-bit v7.x)
- **API Key**: `DEEPSEEK_API_KEY` or `DASHSCOPE_API_KEY`

---

## Project Structure

```
.
+-- README.md
+-- test-deepseek.rkt          # DeepSeek demos (env-based)
+-- test-tongyi.rkt            # Tongyi demos (env-based)
+-- net-io.rkt                 # Pure network transport: HTTP + SSE
+-- api-config/                # API URLs, model names, keys
|   +-- deepseek.rkt
|   +-- tongyi.rkt
+-- api-platform/              # Platform adapters (pluggable)
|   +-- deepseek/
|   |   +-- chat.rkt
|   |   +-- json-build-parse.rkt
|   |   +-- provide.rkt
|   +-- tongyi/
|       +-- chat.rkt
|       +-- json-build-parse.rkt
|       +-- provide.rkt
+-- env/                       # Environment abstraction (NEW)
|   +-- deepseek-env.rkt       # DeepSeek env (with thinking/reasoning)
|   +-- tongyi-env.rkt         # Tongyi env
+-- tools/                     # Tool system
|   +-- tool.rkt               # Tool framework
|   +-- deepseek-base-tool.rkt # Built-in: run_shell, read_file, write_file
+-- history/                   # Conversation history manager
+-- format-color/              # Terminal output styling
```

---

## License

MIT
