# Haskell SAT examples

Some examples of expressing problems in propositional logic and
generating an SMT-LIB script for them:

- [Hamiltonian path](./hamiltonian-path-sat.hs)
- [Graph coloring](./graph-coloring-sat.hs)

```sh
# Requires ghc and Z3
z3 <(runghc hamiltonian-path-sat.hs)

# You can also save the generated SMT-LIB code as follows:
runghc hamiltonian-path-sat.hs > hamiltonian-path-sat.smt
```
