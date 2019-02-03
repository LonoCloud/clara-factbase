# Architecture

Datomic provides [four indexes](https://docs.datomic.com/on-prem/indexes.html) for datoms:
* EAVt (all datoms)
* AEVt (all datoms)
* AVEt (attributes of :db/unique :db/index)
* VAEt (attribute :db.type/ref)

Because query is largely governed by Clara (particularly as it relates to
indexinwe only interested in the indexing) we are primarily interested in
transaction-side aspects of schema (the exception to this is :db.isComponent
which is useful for pull functionality)

## Schema

### `::cardinality`
#### `::cardinality/one`
Upsert given entity+attribute to value.
#### `::cardinality/many`
Addition of non-repeated entity/attribute values.

### `::valueType`
If schema checking is off, this is ignored except for `::type/ref` which will
be used for adding to the.
- require schema for all attributes
- take

### `::unique`
#### `::unique/identity`
Entity upsert during tranaction — must be done across all EAVs at once
#### `::unique/value`
Transaction error if more than one attr/val is being attempted to be inserted.

### `::isComponent`
Used in pull.
Used in retract.
