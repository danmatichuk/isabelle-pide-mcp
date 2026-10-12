---
name: isabelle-ml
description: Developing and exploring code in Isabelle/ML. Use this skill whenever you are working on ML code in Isabelle.
metadata:
  version: "1.0"
---

## What I Do

Guidance for programming and exploration of ML code in Isabelle.

## When To Use Me

Use this skill when working with Isabelle/ML code, typically located in ML files (*.ML) and ML blocks inside Isabelle theories (*.thy).

## Quick Reference

Isabelle/ML is the dialect of Standard ML in Isabelle, primarily used to implement its logical infrastructure (kernel, tactics, etc.).
It communicates with Isabelle/Scala, which manages the "non-logical" part of Isabelle (management of documents, build tools, etc.).

- **Consult the references hierarchy outlined below to obtain contextual knowledge** on Isabelle/ML. You may do so repeatedly when working on larger projects.
- **Use Isabelle's coding and documentation styles and standards and its library functions**. 
  You can find them in Isabelle's home directory sources and in the manuals (see below).
- **Naming standards are particularly important as is sticking to Isabelle/ML's jargon**, including in comments and when communicating with the human user.
- **Use comments sparingly**, comparable to the distribution.
- Besides being in line with the distribution, **your code must be correct, efficient, intelligible, modular, and maintainable**.
- **When working on a big new project, first thoroughly review the standards and the library. For small changes, the existing local context can be enough.**

## References Hierarchy

**Consult the following references in Isabelle's home folder in the order given below until you arrive at a clear answer.
Note that all these references can be very useful**, not just the top entries.
Also use these sources (including author names of commits) when communicating with the human user.
Only speculate about the intent of a piece of code if the following do not provide a definite answer and be transparent about that.

1. Documentation:
  - `NEWS`: human-readable changelog, grouped by Isabelle release.
  - `src/Doc/Implementation/`: the main manual for Isabelle/ML
  - `src/Doc/Isar_Ref/`: the manual for Isar and its main commands.
  - Other manuals in `src/Doc/` (e.g. `Functions`, `Datatypes`, `Locales`): use as appropriate when requiring expert knowledge in these areas.
2. ML sources: Look at the implementation and usage sites of concepts.
   The latter often clarifies (implicit) invariants that have to be preserved by callers.
3. Mercurial history: The decade-old history often explains why code looks the way it does (including important details) and who is responsible for it.
   Affected files of a commit typically provide useful context.

## Isabelle/ML Jargon Overview (non-exhaustive)

| Term | Meaning |
|------|---------|
| Isar | Isabelle's surface language |
| context (`Proof.context`, `Context.generic`) | Immutable record containing state and data |
| local theory (`local_theory`) | proof contexts for applications inside a (named) target |
| theory (`thy`) | Top-level context of a theory |
| morphism | Bundle of structure-preserving maps between contexts |
| attribute | `thm * context` transformation functions. Used in Isar via `[<attributes>]` |
| tactic | `thm -> thm Seq.seq` functions operating on proof states |
| method | Context-aware tactic that is registered as a proof method in Isar |
| rule | Theorem used in an inference step |
| schematic | Something containing instantiable variables `?x`. These variables are called "schematic" ("meta variables" in some literature) |
| oracle | Dynamically introduced axiom, bypassing the kernel |
| antiquotation (`@{some_antiquot ...}`, \<^some_antiquot>‹...›`) | compile-time splice in ML or document source |

