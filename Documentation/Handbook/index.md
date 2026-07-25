# portage-ng Handbook

A comprehensive guide to portage-ng — a declarative reasoning engine for
software configuration, applied to Gentoo Linux.

## Part I — Getting Started

1. [Introduction](01-doc-introduction.md)
2. [Installation and Quick Start](02-doc-installation.md)
3. [Configuration](03-doc-configuration.md)

## Part II — Architecture and Internals

4. [Architecture Overview](04-doc-architecture.md)
5. [Proof Literals](05-doc-proof-literals.md)
6. [Knowledge Base and Cache](06-doc-knowledgebase.md)
7. [The EAPI Grammar](07-doc-eapi-grammar.md)
8. [The Prover](08-doc-prover.md)
9. [Assumptions and Constraint Learning](09-doc-prover-assumptions.md)
10. [Version Domains](10-doc-version-domains.md)
11. [Rules and Domain Logic](11-doc-rules.md)
    - [Policy cards](Policy/README.md) — what Gentoo policy requires
    - [Policy by example](Policy/examples.md) — overlay curriculum
    - [One-page map](Policy/map.md) — `rule/2` → schema → test → card
12. [Planning and Scheduling](12-doc-planning.md)
13. [Output and Visualization](13-doc-output.md)

## Part III — Features

14. [Command-Line Interface](14-doc-cli.md)
15. [Building and Execution](15-doc-building.md)
16. [Semantic Search and LLM Integration](16-doc-llm.md)
17. [Distributed Proving](17-doc-distributed.md)
18. [Upstream and Bug Tracking](18-doc-upstream-bugs.md)
19. [Gentoo Linux Security Advisories (GLSA)](19-doc-glsa.md)

## Part IV — Foundations

20. [Contextual Logic Programming](20-doc-contextual-logic-programming.md)
21. [Context Terms and Feature Unification](21-doc-context-terms.md)
22. [Resolver Comparison](22-doc-resolver-comparison.md)
23. [Dependency Ordering](23-doc-dependency-ordering.md)

## Part V — Development

24. [Testing and Regression](24-doc-testing.md)
25. [Performance and Profiling](25-doc-performance.md)
26. [Contributing](26-doc-contributing.md)

## Closing

27. [Closing Thoughts](27-doc-closing.md)
