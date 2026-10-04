# nil

A minimal coding agent for the terminal. Works with any OpenAI-compatible API
(OpenAI, OpenRouter, Ollama, ...).

    pnpm install && npm link    # uninstall: npm unlink -g nil

Set up the environment:

    NIL_API_KEY=sk-...                         # required
    NIL_BASE_URL=https://openrouter.ai/api/v1  # default: https://api.openai.com/v1
    NIL_MODEL=qwen/qwen3.8-27b:free            # default: gpt-5.4-nano

`NIL_BASE_URL` must be an OpenAI-compatible endpoint, without the
trailing `/chat/completions`. Pick a model that supports tool calling.

Then:

    nil                   new session (prints its id)
    nil --resume          continue the last session
    nil --resume <id>     continue a specific session (id prefix works)
    nil --auto            don't ask before running tools

By default nil asks before it writes files or runs commands.
Sessions live in `./.nil/`. `/help` lists commands, like `/model` to switch models.
