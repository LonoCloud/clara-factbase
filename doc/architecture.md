# Architecture

## Namespace dependency structure

clara-eav.rules
  clara-eav.eav
  clara-eav.session
    clara-eav.store
      clara-eav.eav
  clara-eav.store
    clara-eav.eav
  clara-eav.dsl
  
## High level

- session wrapper contains:
  - clara session
  - EAV store sync'd with session
  - metadata options
    - schema
    - schema-mode (ignore/warn/enforce) - are attributes in the schema
    - typecheck-mode (ignore/warn/enforce) - do values match spec in schema for attr
    - tx-overwrite-mode (ignore/warn/enforce) - can you overwrite card-one EAVs without retracting first
    - no-retract-mode (ignore/warn/enforce) - can you ever retract or modify any EAVs (UNIMPLEMENTED)

## Schema

Datomic provides [four indexes](https://docs.datomic.com/on-prem/indexes.html) for datoms:
* EAVt (all datoms)
* AEVt (all datoms)
* AVEt (attributes of :db/unique :db/index)
* VAEt (attribute :db.type/ref)

Because query is largely governed by Clara (particularly as it relates
to indexing) we are primarily interested in transaction-side aspects
of schema (the exception to this is ::db/isComponent which is useful
for pull functionality).

For the inital work, we are also ignoring the transaction entity (t)
which is not (currently) supported by clara-eav.

Since we are primarily concerned with the transaction side, the AVE(t)
index is actually split out into its two use cases, one `AV->EAV` index
for tracking ::unique/identity attribute-values and another for
tracking ::unique/value attribute-values.

### `::cardinality`

#### `::cardinality/one`
Upsert given entity+attribute to value. If the attribute already
exists on an entity, replace it (retract, assert) with the new value.

#### `::cardinality/many`
Addition of non-repeated entity/attribute values. Regardless of the
existance of the attribute on the entity, assert an additional
attribute-value on the entity.

### `::valueType`
If schema checking is off, this is ignored except for `::db/ref`
attributes which will resolve any tmpid eids during transactions.

#### `::db/ref`
1. A tmpid in the value position (for a ::db/ref attribute) and also
   appears in the entity position and that entity position gets
   assigned a temporary ID during the tx.
   A tmpid in the ref value position unifies with a new new-eid.
2. A tempid in the ref value position unifies with a fixed eid
  a. one explicit in tx
  b. on that's coming from the DB
3. A tempid in the ref value position does not unify with anything
   other than other tmpids.

Interations with :unique/identity:

- upsert map for eids done on :unique/identity AV pairs
- check for multi-eid collissions
- for tmpids in value position (:db/ref), substitute with max tmpid or
  found eid from upsert map
- if substitutions done, repeat whole process
- if no substituions done, assign new eids to upsert map and
  substitute across all E and V position tmpids... done

The full :db/ref interactions are:

- :cardinality/many
  - :type is not :db/ref
  - :type is :db/ref
- :cardinality/one
  - not :unique
    - :type is not :db/ref
    - :type is :db/ref
  - :unique/identity
    - :type is not :db/ref
    - :type is :db/ref
  - :unique/value
    - :type is not :db/ref
    - :type is :db/ref


### `::unique`
#### `::unique/identity`

Entity upsert during tranaction — must be done across all EAVs at once

#### `::unique/value`
Transaction error if more than one attr/val is being attempted to be inserted.

### `::isComponent`
Used in pull.
Used in retract.

# Questions

- Do we want to allow tempid for entity ID during retraction for :unique/identity attributes



# TODO

- We do not currently distinguish between Clojure #{} sets as values and as :cardinality/many #{} syntax and storage (maybe mark with metadata or reify unique type?)
- Maybe this is ambiguity starting with the syntax. Do we require card-many values to be expressed as #{val...} or can bare values sometimes be expressed. And does this leak over to the EAV triple syntax.


## THINKING

( R = NOOP, unless E AV or t AV,:uniq/id )
( A = ...)

R [E  AV]            -> remove EAV
R [E  av]            -> NOOP
R [e  av]            -> NOOP
R [e  AV] :uniq/id   -> NOOP
R [e  AV] :uniq/val  -> NOOP?
R [E2 AV] :uniq/id   -> NOOP?
R [E2 AV] :uniq/val  -> NOOP?

            v--- Datascript errors
R [t  av]            -> NOOP
R [t  AV]            -> NOOP
R [t  AV] :uniq/id   -> resolve id, remove EAV
R [t  AV] :uniq/val  -> NOOP or ERROR?


A [E  AV]            -> NOOP
A [E  Av] :card/one  -> retract EAV, assert EAv
A [E  Av] :card/many -> assert  EAv
A [e  av]            -> assert (well, only if the e is one we have seen before, otherwise we go past valid entity ids in Datomic)
A [e  AV] :uniq/id   -> ERROR
A [e  AV] :uniq/val  -> ERROR
A [E2 AV] :uniq/id   -> ERROR
A [E2 AV] :uniq/val  -> ERROR

A [t  av]            -> new     id, assert
A [t  AV]            -> new     id, assert
A [t  AV] :uniq/id   -> resolve id, NOOP
A [t  AV] :uniq/val  -> ERROR

:type/ref

R [E A E] 
R [E A E] :uniq/id
R [E A E] :uniq/val

R [E A e] 
R [E A e] :uniq/id
R [E A e] :uniq/val
R [E A t]
R [E A t] :uniq/id
R [E A t] :uniq/val
R [e A E]
R [e A t]

A [E A E]
A [E A t]
A [e A E]
A [e A t]
 
