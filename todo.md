have a look at outdated instructions

have a look at demo script

fix devcontainer

try it with gpt 5 codex mini

runtime logs to make sense of non-functionial requirements

spec kit for every agent

save agents prompts in prompt files to make them easier to edit and reuse

ghcp sdk?

run-chunked / chunked (aliases) (doctor commands) check again after merge
Irrelevant / dead code. In the main() router, run-chunked and chunked are mapped to run_migration (the standard run), not to run_migration_chunked(). That means the entire run_migration_chunked() function (~200 lines) is dead code — never called. It also passes --chunked to dotnet, which is not a recognized CLI flag. Should be removed entirely.