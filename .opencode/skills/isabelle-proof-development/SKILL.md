---
name: isabelle-proof-development
description: Developing proofs in the Isabelle proof assistant. Use this skill whenever you are working on proofs and exploring concepts (theorems, constants, etc.) in Isabelle.
metadata:
  version: "1.0"
---

## What I Do

Guide proof search, automation, and concept search in the Isabelle proof assistant.

Before starting, make sure the PIDE MCP works (load the `pide-mcp` skill, section "Before You Start"); if it does not, tell the user instead of working around it with batch builds.

## When To Use Me

Whenever you need to:
- Think about automation and theorem tagging.
- Close a goal that you could not one-shot.
- Decide which automation to use (`simp` vs `auto` vs `blast` vs `sledgehammer` vs ...).
- Search for existing theorems and constants.

## Quick Reference

- Use automation tags (`simp`, `intro`, `elim`, etc.) on theorems, but be wise about them.
- When a goal doesn't close with an obvious guess, **your default move is to use `try0`**. When `try0` fails, **use `sledgehammer`** (poll its output!). 
- When automation is brittle or explodes (non-determination): switch to structured Isar proofs and add intermediate steps.
- When unsure whether a lemma exists or what lemmas exist in general, **use `find_theorems`** to search in all transitively imported theories. **Unsure of the exact name? Search by part of it: `name: rev`**; the name search is the replacement for grepping for a lemma name.  
- When unsure whether a constant exists or what constants exist in general, **use `find_consts`** to search in all transitively imported theories.  
- When searching for concepts in other theories, import the theory if it is from the base session or it is a small dependency; otherwise use grep (since it avoids building the dependency).

IMPORTANT:
- **`try0`, `find_theorems`, and `find_consts` are very cheap and can be used frequently.**
- **`sledgehammer` should be used whenever `try0` failed. It is almost always cheaper than guessing a proof in several attempts.**
    `try0` and `sledgehammer` are for when you **do not know why** a method failed or what would close the goal. If you do know (a rewriting-shape problem you can read off the goal, a missing rule that `find_theorems` found, a missing class), fix that directly instead of running them first; but do not guess variants of a method.
    Don't guess proofs and proof method invocations (particularly using `metis`) that you could not one-shot: `sledgehammer` typically finds these in seconds.
- **Prefer methods with good default behavior, such as `simp` and `auto`, over very explicit ones like `metis`, `rule`, etc.**

## Goals of the form `A &&& B`

A goal shown as `A &&& B` (meta-conjunction, written by you or produced by `lemma shows A and B`) is several subgoals in one. **Applying any method splits it, even the no-op `apply -`.** Observed with `lemma "(x::nat) = x &&& (y::nat) = y"`: before, `goal (1 subgoal): 1. x = x &&& y = y`; after `apply -`, `goal (2 subgoals): 1. x = x 2. y = y`. The same happens for `lemma l: shows "x = x" and "y = y"`.

- **Do not use `rule conjI` or `rule conjunctionI` on it.** A method splits the chain first and then acts on subgoal 1: `apply (rule refl)` on `x = x &&& y = y` succeeds and leaves `1. y = y` (checked), while `rule conjI` fails because subgoal 1 is not a conjunction. **A `Failed to apply proof method` error shows the goal from before the method**, i.e. the unsplit chain, so that failure does not mean the chain could not be split (a `Failed to finish proof` error from `by`/`done` shows instead the state where the proof got stuck, after its methods ran). (`conjI` is for the object-level `A ∧ B`, which it splits as usual.)
- **Do:** `apply -`, then handle each subgoal in order, or let `simp_all` / `auto` / `try0` work on all subgoals at once.
- **Goals of other `thy_goal` commands are not `&&&` goals.** `instance` gives `1. OFCLASS(nat, two_ax_class)` and `interpretation` gives `1. two (\<lambda>x. x)`: one subgoal that `apply -` does not split (checked). `apply standard` (instance) or `apply unfold_locales` (interpretation) turns it into one subgoal per axiom (checked: two subgoals each).

## Basic idioms and traps (each checked in a scratch theory)

- **Closing a proof with nothing left to prove:** `using a .` closes a goal that the chained facts are exactly (`.` is `by this`); `using a b ..` closes `A ∧ B` with one standard rule; `instance nat :: c ..` does it for a class without axioms; `by -` also closes a goal from chained facts; after `apply` steps that show `No subgoals!`, finish with `done`.
- **An unknown identifier is not an error, it is a free variable with an inferred type:** `lemma "lenght xs = lenght xs" by simp` is accepted without warning (and proves nothing about `length`), while `lemma "lenght [a, b] = 2"` is simply unprovable. A typo or a missing import shows up as an unprovable goal: check the constant with `find_consts name: ...`.
- **`oops` abandons the goal and registers nothing** (afterwards `thm l` says `Undefined fact`). A proof that is left open means the next command fails with `Bad context for command ... -- using reset state`, and the unfinished lemma does not exist for later commands.
- **`simp` fails without saying why when a type variable lacks the class a rule needs:** `lemma "(x::'a::{plus,zero}) + 0 = x"` then `apply simp` gives `Failed to apply proof method` on the goal `x + 0 = x`, which looks exactly like a provable goal. Sorts are hidden in the display; `declare [[show_sorts]]` shows `'a :: {plus,zero}` (`monoid_add` would make it work). Without any class annotation, Isabelle reports a type error instead (`'a::type not of sort plus`).
- **A rewrite rule `g x = ...` only matches an application of `g`; a bare `g` is not rewritten.** In `map g xs` (also `fold g`, `filter P`, ...) `g` is bare, and Isabelle eta-contracts `λx. g x` to `g`, so `map (λx. g x) xs` is displayed as `map g xs`. Checked: with the rule `g x = x`, `simp add: g_app` fails on `map g xs = xs`; proving the function-level equation `g = id` (`by (rule ext) (simp add: g_app)`) lets `simp add: g_fun` close it. In the same way `id x = x` does not apply to the bare `id` in `map id xs` (the simp rule `map_ident` does).
- **Induction over a nested datatype** (`datatype 'a rose = Node 'a "'a rose list"`) gives an induction hypothesis conditional on membership, `(⋀x. x ∈ set ts ⟹ P x)`. Plain `simp` cannot use it: for `mapr id t = t` the goal after `apply (induct t)` and `apply simp` is still `map (mapr (λa. a)) ts = ts` (note that `simp` also rewrote `id` to `λa. a`). `simp cong: map_cong` or a lemma such as `map_idI` closes it (both checked).
- **A looping simp does not fail, it keeps running:** with a simp rule `ff x = ff (ff x)`, `by simp` stayed `still_running_possibly_nonterminating` for more than 40 seconds; testers saw `exception Interrupt_Breakdown raised` once such a command was interrupted. Do not wait for it (`cancel` stops it): remove the rule you added last (e.g. from `simp_all add: ...`) or try plain `simp` / `simp_all` first.
- **`Undeclared class: "??.len"`:** the `??.` prefix means the theory defining the class is not imported. Import it; find it with `find_consts` or by grepping the session directories from `get_session_state`.

## Theorem Tagging

- Use automation tags (`simp`, `intro`, `elim`, etc.) on theorems, but be wise about them.
- For `simp` theorems: avoid loops (possibly involving other simp rules) and pay attention to obtain good normal forms.
- Correctly classify rules for the classical reasoner (intro, elim, dest).
- **Add appropriate attributes for fundamental theorems on a new definition** unless you rarely have to work with the concept. In that case, manually pass the rules to the provers.

## How to use (Proof) Search Commands

1. Insert the command at the right position: typically, after the goal opens.
2. Poll the state/output at the command position. Respect the maximum timeouts mentioned in this skill file (short ones: do not wait longer for `sledgehammer`).
3. When the command returns the needed output, take it. Then remove the command.
4. If the command doesn't return results after the maximum timeout, also remove the command.

## Default Proof Search Order and Timeouts

**Order for closing a goal that you couldn't prove in one-shot/immediately:**
1. `try0`: runs cheap built-in methods (`simp`, `auto`, `blast`, `force`, ...). Usually finishes in **<5 seconds**. This is your first move.
2. `sledgehammer [timeout = 10]`: uses fact selection, built-in methods, and external provers. **Always pass a short timeout** (`[timeout = 10]`): a goal that is solvable usually gets solved within seconds, while an unsolvable one can run (and block) for a long time. 
    Use after `try0` failed and always before trying to generate a proof that you could not one-shot.

## `try0`

You have to pass facts explicitly to `try0`:
```isabelle
using <some theorems> try0 simp: <some simp theorems> intro: <some intro rules> ...
```
Poll until you hit the maximum timeout mentioned in this file and pick the fastest returned proof.

## `suggest_facts`

**`suggest_facts` shows the lemmas that Sledgehammer's relevance filter (MePo/MaSh) selects for the goal**, most relevant first, with their statements, without running any prover (a call takes about a second). `start_line` is a line inside the proof (the goal after that line's command is used); `max_facts` defaults to 20. Needs a HOL-based session.
- Use it when you continue **manually** (e.g. after `sledgehammer` found nothing or timed out): look at the top facts and pass fitting ones to `using`, `simp:`, `intro:`, `rule`.
- Use it to learn which lemmas live around a goal before searching with `find_theorems`.
- It is not a replacement for `sledgehammer`: if you only want a proof, run sledgehammer.

## `sledgehammer`

- **Insert `sledgehammer [timeout = 10]`, never a bare `sledgehammer`.** Poll after 5 seconds, then after 10, and give up after about 15-20 seconds in total. If there is still no result, the goal is probably not within reach as stated: **remove the command** (removing it cancels the run) and do not retry with a longer timeout.
- **A hanging or empty sledgehammer is information, not a reason to wait.** Instead, change the situation: split the goal into smaller steps (`have`), add the missing facts with `add: <facts>` or `using`, try `try0` with explicit facts, or first prove a helper lemma. Only then run sledgehammer again, again with a short timeout.
- If a `sledgehammer` tool is available, use it instead of inserting the command and pass a short `timeout` (e.g. 10): it does not modify the file.
- Pick the **fastest reconstruction with the simplest method**: prefer `auto`, `fastforce`, etc. over `metis` and `metis` over `smt`. 
- **Don't use too many parallel sledgehammers**. Instead, insert just a few (<=5) and iterate.
- `sledgehammer` has built-in fact selection: it often finds the right lemmas. 
- **Only transitively imported theorems can be queried by sledgehammer.** If you suspect some currently non-imported theory contains useful material, you first have to import it.
    It is much quicker to let sledgehammer find the right lemmas than to search for them on your own.

## `find_theorems`

Searches loaded theorems.
**Use the `find_theorems` MCP tool** (argument `query`, optional `limit`, `start_line` for goal-dependent criteria): it does not modify the file and answers in a call. **Do not grep the sources for theorems that are imported**: `find_theorems` is faster, matches by pattern, and finds the exact statement. Inserting the Isar command `find_theorems` into a theory is only the fallback if the tool is not available. Use grep for theories that are not imported (see above).
Syntax of the query (the same as after the Isar command): `<criterion> <criterion> ...`; the tool takes the count as `limit`.

| Criterion | Syntax | Matches |
| --- | --- | --- |
| Pattern | `"<term>"` | Theorems containing a matching subterm. **Default criterion.** |
| Name | `name: <ident>` | Theorems whose name contains `<ident>`. |
| Intro / Elim / Dest | `intro` / `elim` / `dest` | Rules applicable to the *current goal*. |
| Solves | `solves` | Theorems that directly close the current goal. |
| Simp | `simp: "<pattern>"` | Simp rules rewriting the given pattern. |

Each criterion can be negated, e.g. `-name: foo`, `-"rev (rev _) = _"`, etc.
**Patterns are matched.** Use `?xs` (schematic) or `_` (wildcard) for variables that can be instantiated. Examples:

```isabelle
find_theorems "rev (rev ?xs) = ?xs"      (* ✅ *)
find_theorems "rev (rev _) = _"          (* ✅ *)
find_theorems "rev (rev xs) = xs"        (* ❌ xs is a free variable; returns nothing *)
find_theorems name: rev "?xs @ _"        (* ✅ combined criteria *)
find_theorems intro                      (* ✅ intro rules for current goal *)
find_theorems 5 "rev (rev ?xs) = ?xs"    (* ✅ cap at 5 results *)
```

## `find_consts`

**Use the `find_consts` MCP tool** (argument `query`), not grep, for constants of imported theories.

Searches loaded constants.
Syntax: `find_consts <criterion> <criterion> ...`

| Criterion | Syntax | Matches |
| --- | --- | --- |
| Type | `"<type pattern>"` | Constants whose type contains a matching subtype. **Default criterion.** |
| Strict type | `strict: "<type pattern>"` | Constants whose type matches |
| Name | `name: <ident>` | Constants whose name contains `<ident>`. |

**Polymorphic types `'a` are matched (they can be instantiated)**. Examples:

```isabelle
find_consts "nat"                    (* any constant whose type contains `nat` *)
find_consts strict: "nat"            (* constants of type exactly `nat` *)
find_consts "'a list ⇒ 'b list"      (* also finds `nat list ⇒ bool list` *)
find_consts name: rev                (* by name *)
find_consts name: list "_ ⇒ nat"     (* combined criteria *)
```

## Limitations of `find_theorems` and `find_consts` and when to use `grep`

**Only transitively imported theories can be queried by these commands.**
When searching for concepts in other theories, import the theory if it is from the base session or it is a small dependency; otherwise, use `grep` (since it avoids building the dependency).

