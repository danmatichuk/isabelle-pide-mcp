---
name: isabelle-proof-development
description: Developing proofs in the Isabelle proof assistant. Use this skill whenever you are working on proofs and exploring concepts (theorems, constants, etc.) in Isabelle.
metadata:
  version: "1.1"
---

Proof search, automation and concept search in Isabelle. Make sure the PIDE MCP works first (`pide-mcp` skill, "Before You Start"); if it does not, tell the user instead of working around it with batch builds.

## Closing a Goal

**When a goal does not close with an obvious guess:**
1. **`try0`** (cheap built-in methods, usually under 5 s). Pass facts explicitly: `using <thms> try0 simp: <thms> intro: <rules>`; take the fastest proof.
2. **`sledgehammer [timeout = 10]`**, always with a short timeout (the `sledgehammer` tool with `timeout: 10` if available: it does not modify the file). Solvable goals are usually solved within seconds. Give up after 15-20 s and remove the command; do not retry with a longer timeout.
3. **A hanging or empty sledgehammer is information, not a reason to wait:** split the goal (`have` steps), add facts (`using`, `add:`), prove a helper lemma, then try again. `suggest_facts` shows the lemmas Sledgehammer considers relevant, for continuing manually.

`try0` and `sledgehammer` are for when you **do not know why** a method failed. If you do know (a rewriting shape you can read off the goal, a rule `find_theorems` found, a missing class), fix that directly; but never guess variants of a method, particularly `metis`: sledgehammer finds those in seconds. Prefer the simplest reconstruction (`simp`, `auto`, `fastforce` over `metis`, `metis` over `smt`), and run at most a few sledgehammers in parallel. Sledgehammer only sees transitively imported theories.

When automation is brittle or explodes, switch to a structured Isar proof with intermediate steps.

## Searching Theorems and Constants

- **Use the `find_theorems` / `find_consts` tools, not grep, for imported theories:** they are cheap, match by pattern and give exact statements. Unsure of a name? Search part of it: `name: rev`.
- **Patterns are matched: use `?xs` or `_` for variables.** `"rev (rev ?xs) = ?xs"` finds the lemma, `"rev (rev xs) = xs"` (free `xs`) finds nothing. Criteria combine and negate (`name: rev "?xs @ _"`, `-name: foo`); `intro`, `elim`, `dest`, `solves` use the goal at `start_line`. In `find_consts`, `'a` matches any type (`"'a list ⇒ 'b list"` also finds `nat list ⇒ bool list`); `strict:` matches exactly.
- Only transitively imported theories are searched. For others, import the theory if it is from the base session or small, otherwise grep.

## Basic idioms and traps (each checked in a scratch theory)

- **Goals `A &&& B`:** see the `pide-mcp` skill (split with `apply -`, not `rule conjI`).
- **Closing a proof with nothing left to prove:** `using a .` closes a goal that the chained facts are exactly; `using a b ..` closes `A ∧ B` with one standard rule; `instance nat :: c ..` for a class without axioms; after `apply` steps that show `No subgoals!`, `done`.
- **An unknown identifier is not an error, it is a free variable:** `lemma "lenght xs = lenght xs" by simp` is accepted (and proves nothing about `length`), `lemma "lenght [a, b] = 2"` is just unprovable. A typo or missing import shows up as an unprovable goal: check the constant with `find_consts name: ...`.
- **`oops` registers nothing.** A proof left open makes the next command fail with `Bad context for command ... -- using reset state`, and the lemma does not exist for later commands.
- **`simp` fails without a reason when a type variable lacks a class a rule needs:** for `(x::'a::{plus,zero}) + 0 = x`, `apply simp` fails on a goal that looks provable; `declare [[show_sorts]]` shows the sorts (`monoid_add` would work).
- **A rewrite rule `g x = ...` does not rewrite a bare `g`,** as in `map g xs` (also `fold g`, `filter P`); Isabelle displays `map (λx. g x) xs` as `map g xs`. Prove the function-level equation (`g = id` by `rule ext`) and simp with that.
- **Induction over a nested datatype** (`datatype 'a rose = Node 'a "'a rose list"`) gives the hypothesis `(⋀x. x ∈ set ts ⟹ P x)`, which plain `simp` cannot use: `simp cong: map_cong` or a lemma like `map_idI` closes such goals.
- **A looping simp does not fail, it keeps running** (`still_running_possibly_nonterminating`; once interrupted, `exception Interrupt_Breakdown raised`). Do not wait for it (`cancel` stops it): remove the rule you added last, or try plain `simp` / `simp_all` first.
- **`Undeclared class: "??.len"`:** the theory defining the class is not imported.

## Theorem Tagging

- Use automation attributes (`simp`, `intro`, `elim`, `dest`) wisely: simp rules must not loop (also together with other simp rules) and should give good normal forms; classify classical rules correctly.
- Give the fundamental theorems of a new definition suitable attributes, unless the concept is rarely used: then pass the rules to the provers explicitly.
