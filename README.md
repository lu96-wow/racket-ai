# AI DSL — Fully Composable Building Blocks for AI Conversations

> **Everything is composable from the ground up.**  
> This is not a framework. It is a set of zero-coupling building blocks — network transport, platform adapters, tool system, conversation history, and terminal styling — each with a unified interface, ready to be composed into any form of AI interaction.  
> `ai-dsl.rkt` is merely a **sample implementation** built on top of these blocks, never a requirement.

**License:** MIT

---

## One Minute to Start: The Minimal Chat

8 lines of code. No framework. Just function composition:

```racket
#lang racket
;; Pick only the blocks you need. Ignore the rest.
(require "api-platform/deepseek/chat.rkt"
         "api-platform/deepseek/json-build-parse.rkt"
         "api-config/deepseek.rkt")

;; 1. Compose: one message → one request
(define req
  (build-chat-request
   (hasheq 'model deepseek-v4-flash
           'messages (build-messages
                      (build-user-message #:content "Introduce Racket in one sentence."))
           'max_tokens 256)))

;; 2. Send, parse, display
(define resp (deepseek-chat req))
(display (response-content resp))
```

Run it:

```bash
export DEEPSEEK_API_KEY="sk-..."
racket my-first-chat.rkt
```

**The core advantage is already on full display:**
- `build-user-message` → `build-chat-request` → `deepseek-chat` → `response-content` — every layer is a pure function, independently replaceable, testable, reusable
- Want to switch platforms? Just change the `require` paths and the API key env var
- Want streaming? Add `'stream #t` to the hash table, switch to `deepseek-chat/stream`, pass a callback
- Want tools? Add `'tools` + `tool-dispatch`
- **Struggling to remember what parameters are available?** Call `chat-request-keys` or `message-keys` at runtime — they return the full list of valid keys.

**This is what "everything is composable" means.** No magic, no implicit dependencies. Just straightforward expression composition.

---

## Building Blocks at a Glance

```
┌──────────────────────────────────────────────┐
│  test.rkt  ← Best starting point, 5 demos    │
├──────────────────────────────────────────────┤
│  ai-dsl.rkt  ← Sample REPL (optional)        │
├──────────────────────────────────────────────┤
│                                              │
│  Core Building Blocks (freely composable)     │
│                                              │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐     │
│  │  Tools   │ │ History  │ │ Terminal │     │
│  │  system  │ │ manager  │ │ styling  │     │
│  │ tools/   │ │ history/ │ │ format-  │     │
│  │          │ │          │ │ color/   │     │
│  └──────────┘ └──────────┘ └──────────┘     │
│                                              │
│  ┌──────────────────────────────────┐        │
│  │ Platform Adapters (pluggable)    │        │
│  │ api-platform/deepseek/           │        │
│  │ api-platform/tongyi/             │        │
│  │ — same interface, interchangeable │        │
│  └──────────────────────────────────┘        │
│                                              │
│  ┌──────────────────────────────────┐        │
│  │ Network Transport net-io.rkt     │        │
│  │ pure HTTP + SSE, no platform     │        │
│  │ logic                            │        │
│  └──────────────────────────────────┘        │
│                                              │
│  ┌──────────────────────────────────┐        │
│  │ API Config api-config/           │        │
│  │ URLs, model names, API keys      │        │
│  └──────────────────────────────────┘        │
└──────────────────────────────────────────────┘
```

Every layer is an independent Racket module. Zero coupling, zero implicit dependencies.

---

## More Composition Examples

### Streaming Chat

```racket
(define req (build-chat-request
             (hasheq 'model deepseek-v4-flash
                     'messages messages
                     'stream #t
                     'max_tokens 256)))

(deepseek-chat/stream req
  (cons 'content (λ (c) (display c) (flush-output))))
```

### Tool Loop (AI auto-invokes Shell / file read & write)

```racket
;; See test.rkt example 5: streaming + automatic tool loop
(require "tools/deepseek-base-tool.rkt")  ;; built-in run_shell / read_file / write_file

;; Just attach tool schemas to the request — AI decides when to call them
(define req (build-chat-request
             (hasheq 'model deepseek-v4-flash
                     'messages messages
                     'stream #t
                     'max_tokens 8192
                     'tools (tools-schemas default-tools))))
```

### Switch Platform to Tongyi (Alibaba Qwen)

```racket
(require "api-platform/tongyi/chat.rkt"
         "api-platform/tongyi/json-build-parse.rkt"
         "api-config/tongyi.rkt")

;; Different function names, but identical interface signatures
(tongyi-chat req)
(tongyi-chat/stream req ...)
```

---

## Design Decisions

### Hash Table Parameters (Runtime Parameter Discovery)

`build-chat-request` and `build-message` accept a **single hash table** instead of keyword arguments:

```racket
(build-chat-request (hasheq 'model m 'messages msgs 'stream #t 'max_tokens 256))
```

**Why?** Keyword arguments with defaults hide what parameters are available — callers must read the source to discover them. A hash table is self-describing and introspectable:

```racket
;; Discover available parameters at the REPL:
chat-request-keys
;; => '(model messages stream max_tokens temperature top_p
;;     n thinking reasoning_effort tools stop)

message-keys
;; => '(role content reasoning_content tool_calls tool_call_id name)
```

**Default value strategy:** Optional parameters default to `#f` via `hash-ref` and are omitted from the JSON request when absent, letting the API server apply its own defaults. Required parameters (`model`, `messages`) raise a clear error if missing.

**Convenience wrappers** (`build-user-message`, `build-assistant-message`, `build-tool-result`) still accept keyword arguments — they build a hash table internally and delegate to `build-message`.

### `#:stop?` Default in Stream Functions

`deepseek-chat/stream` and `tongyi-chat/stream` accept an optional `#:stop?` predicate to signal early termination:

```racket
(deepseek-chat/stream req
  #:stop? (λ () (some-condition))
  (cons 'content (λ (c) (display c))))
```

The default is `(λ () #f)` — never stop. This avoids a "not a procedure" error when `#f` is passed to the underlying `in-sse` which expects a callable predicate.

---

## Learning Path

| Step | File | Content |
|------|------|---------|
| 1 | `test.rkt` examples 1–2 | Non-streaming / streaming basic chat |
| 2 | `test.rkt` example 3 | Streaming + reasoning (thinking) mode |
| 3 | `test.rkt` example 4 | Non-streaming + tool calls |
| 4 | **`test.rkt` example 5** | **Streaming + automatic tool loop (core pattern)** |

---

## Requirements

- **Racket** (recommended v8.6+, minimum 64-bit v7.x)
- **API Key**: `DEEPSEEK_API_KEY` or `DASHSCOPE_API_KEY`

---

## Going Further: Orchestrate Multiple AIs, Custom Workflows

All the building blocks above naturally lead to a powerful possibility: **orchestrating multiple AI models — each specialized for a particular stage — into a custom AI pipeline for a specific task.** For example, use Model A for intent recognition, Model B for code generation, and Model C for review and summarization. The output of each step can be passed losslessly to the next.

This platform is **always modifiable** — switch models, tweak parameters, change workflows, all within a few lines of code. No framework changes, no architecture overhauls. For different scenarios (customer service, coding assistant, writing aid, etc.), you can quickly assemble a tailored AI pipeline like building with LEGO blocks. This is not a fixed product — it is **your AI workbench, your rules.**
(lambda to ai)
---

## Project Structure

```
.
├── README.md                   # This file
├── test.rkt                    # Best starting point — 5 demo examples
├── ai-dsl.rkt                  # Sample interactive REPL (optional)
├── net-io.rkt                  # Pure network transport: HTTP + SSE
├── api-config/                 # API URLs, model names, keys
│   └── deepseek.rkt
│   └── tongyi.rkt
├── api-platform/               # Platform adapters (pluggable)
│   ├── deepseek/
│   │   ├── chat.rkt            # Chat API (sync + stream)
│   │   ├── json-build-parse.rkt# Request/response builders & parsers
│   │   └── ...
│   └── tongyi/
│       ├── chat.rkt
│       ├── json-build-parse.rkt
│       └── ...
├── tools/                      # Tool system
│   └── deepseek-base-tool.rkt  # Built-in tools: run_shell, read_file, write_file
├── history/                    # Conversation history manager
├── format-color/               # Terminal output styling
└── tools/                      # Utility tools
```

---

## License

MIT
