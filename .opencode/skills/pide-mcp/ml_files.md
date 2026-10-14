# ML Files

- **Work on `*.ML` files through the MCP like on theories:** `read`, `edit`, `get_state` and `find_entities` take the file path as `origin`; external edits are synchronised as for theories.
- **An ML file is only processed as part of the theory that loads it** (`ML_file "Foo.ML"`). Load that theory first; until then `get_state` on the ML file only queues it ("not loaded by any theory").
- **The whole ML file is one command** (the `ML_file` command): `get_state` shows one entry whose first message names the failing line (`ML error (line N of …)`); `detail: full` shows the complete message. Dependents are rechecked after every change.
- **A large ML file is reprocessed completely after each edit.** To explore, test the code in a scratch theory with the same imports in `ML ‹…›` blocks (one command each: per-block status, types, output in seconds), or with `eval_ml`; move the finished code into the `.ML` file afterwards.
- **Check syntax before a reload with `check_ml`** (about 0.2 s, no session), e.g. after a scripted change: a reload of a big file and its dependents takes minutes and then only reports the error. `edit` runs it itself on `.ML` files. It checks syntax only, and lines with antiquotations or control characters are not checked (the result says how many), so still use `get_state` after the reload.
- **Do not fall back to `isabelle build` just because the file is ML:** the MCP rechecks incrementally and shows ML errors with their lines.
