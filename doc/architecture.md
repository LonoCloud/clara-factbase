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

