# SAFE: A Declarative Trust Management System with Linked Credentials

`SAFE` is an integrated system for managing trust using a logic-based
declarative language. Logical trust systems authorize each request by
constructing a proof from a context---a set of authenticated logic statements
representing credentials and policies issued by various principals in a
networked system.

A key barrier to practical use of logical trust systems is the problem of
managing proof contexts: identifying, validating, and assembling the
credentials and policies that are relevant to each trust decision. This paper
describes a new approach to managing proof contexts using context linking and
caching. Credentials and policies are stored as certified logic sets named by
secure identifiers in a shared key-value store. `SAFE` offers language
constructs to build and modify logic sets, link sets to form unions, pass them
by reference, and add them to proof contexts. `SAFE` fetches and validates
credential sets on demand and caches them in the authorizer. We evaluate and
discuss our experience using `SAFE` to build secure services based on case
studies drawn from practice: a secure name service resolver, a secure proxy
shim for a key value store, and an authorization module for a networked
infrastructure-as-a-service system with a federated trust structure.

See the [techinical report] (docs/safe-overview.pdf) for a full overview.

## Status
A research prototype of logic-based declarative trust management system
implemented in [Scala] (http://www.scala-lang.org)
