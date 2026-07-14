# Twin-Model Verification for Ogma Monitors

Ogma generates runtime monitors from temporal logic specifications.
This doc describes a dual-model verification pattern.

## Pattern

```
Generator model  ->  CoCoSpec/Lustre spec draft
     down
Gate model (local) ->  verifies temporal operators,
    guarantees have assume scopes, state machines complete
     down
ogma  ->  generates Rust/C/Lustre monitor
```

## Key Invariants

- Every `guarantee` has a corresponding `assume` scope
- Temporal operators used correctly (always, eventually, until)
- State machine transitions are complete (no dead states)
- Output monitor covers the stated property

## Connection to Formal Verification

This pattern mirrors the sorry gate in Lean 4:
a model can declare correctness while producing an incorrect artifact.
An external gate with the spec loaded catches the contradiction cheaply
before full formal verification runs.
