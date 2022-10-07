# Conversion to SQL

Associations are the only thing converted from the AST, entities are just help.

This whole thing runs after a graph search algorithm ensures there are no loops.

The different parts of an association are converted in different ways.

## Headers

1. They are Permissions, in which case they get turned into a CREATE POLICY.
2. They are Definitions, in which case they get turned into a query.

Both Headers, when translated, do two things:

1. Create a context for translating the Predicate, containing the variables-to-tables translation.
2. When making the final SQL, the elements SELECTed are decided by the Headers.

## Predicates

Before we can turn predicates into SQL, we do an intermediate step, searching for values
with fields that should be turned into a JOIN.

### Fields

Every `VVarField` that's not primitive should create a `(Value -> tableName, conditions)` representing the join it requires.
The conditions are saved for later, but the tableName will then replace the VVarField inside the body of the predicate.

Examples:

```
a.b.c.d
where 
    type(a) = A
    type(a.b) = B
    type(a.b.c) = C
    type(a.b.c.d) = D

and the tables of those types are
table({A, B, C, D}) = "{a,b,c,d}_tabl" respectively

There's three VVarField's, because "a" is just a VVar.
In the end, the generated things should be
[
    -- NOT a -> a_tabl cause it's in the Header context
    a.b -> b_tabl
    a.b.c -> c_tabl
    a.b.c.d -> d_tabl
]
and the conditions
[
    a_tabl.b_id = b_tabl.id
    b_tabl.c_id = c_tabl.id
    c_tabl.d_id = d_tabl.id
].join(" AND ")
```

Remember that the Headers created a context, containing translation of variables to their types. **That context should be expanded** with the one created in the previous section.

### Translating the Predicate

Given the context collected from the Header and the VarFields, we can translate the predicate itself to SQL. This is a surprisingly easy part.
The function that does it is `renderPred`. It gets converted to just the part that goes into the WHERE of the query.
