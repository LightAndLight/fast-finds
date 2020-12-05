# fast-finds

20201205

## Introduction

At work, I help maintain a domain-specific programming language that others in the company use 
for data processing and analysis. I'll call the language 'DSL' for short.

The language provides the following function:
```
find : { values : List a, predicate : a -> Bool, result : a -> b, default : b  } -> b
```

`find` says, "get me the first element of a list that satisfies this predicate, and run 
a projection on it. Otherwise, return a default value".

Written in Haskell: 
```
find values predicate result default = 
  maybe default (result . fst) $
  Data.List.uncons (filter predicate values)
```

I've recently been investigating some slow DSL code. The portion of code that dominates the
running time is essentially a loop that calls `find` a bunch of times. The users of DSL have
a pattern where they'll staically define a few large, immutable list of records (collquially known 
as 'lookup tables'), at the start of their program, and use these tables to categorise the their 
data along various axes.

This code is a rough analogue of the problematic DSL code:

```
for x in xs:
  find { 
    values = table1, 
    predicate = \x -> x.a == something && x.b == somethingElse, 
    result = res1, 
    default = def1 
  }
  find { 
    values = table1, 
    predicate = \x -> x.a == something && x.b == somethingElse, 
    result = res2, 
    default = def2
  }
  find { 
    values = table2, 
    predicate = \x -> x.c == anotherThing && x.d == anotherDifferentThing, 
    result = res3, 
    default = def3
  }
  ...
```

The size of `xs` is usually 500-1000, and values like `table1` and `table2` contain 1000-8000 entries.
This means the above code performs a minimum of 1,500 comparisons, and a maximum of around 2,000,000 
comparisons.

I'm not interested in questioning the analysts' approach. They are far from experts in data
structure and algorithm design, and that's okay. For the purposes of this investigation I'm going to 
assume that the way they've written this code meets all their needs except for performance. Given that,
I'm going to think about how improve the performance of their code without asking them to change it.

Many of these repeated calls to `find` involve checking if a particular list element's field has a certain
value (e.g. `find { values, predicate = \x -> x.a == something, result, default }`). My plan of attack
is to improve the worst-case performance of these calls from O(n) to O(log n) by building indexes for large 
lists that are used as lookup tables.

## Desugaring

When it comes to program analysis, `find` is a bit of a silly function. It does "too much", in a sense.
The first step is to transform `find` into what it *really means*.

```
head : List a -> Maybe a
filter : (a -> Bool) -> List a -> List a
maybe : b -> (a -> b) -> Maybe a -> b

find { values, predicate, result, default } =
  maybe default result (head (filter predicate values))
```

## Understanding fusion

`filter` has a cool fusion property: `filter (\x -> q) (filter (\x -> p) xs) = filter (\x -> p x && q x) xs`. (I reversed
the order of `p` and `q` so that `p` is always the first predicate to be checked). Normally we want
to use it in the left-to-right direction, so that we turn two list traversals into one. I'm going
to invoke it the right-to-left direction so that we can "learn" more about out filters. The actual
form of the rule that I'll use is `filter (\x -> ... && p x && ...) = filter (\x -> ... && ...) (filter (\x -> p) xs)`,
to "cherry-pick" a particular predicate.

## Specialising filter

I'll now introduce a new function: `select[x] : a -> List { x : a | r } -> List { x : a | r }`. `select`
is a family of functions that range over record field labels. Here's what it *means*:

```
select[x] value = filter (\v -> v.x == value)
  where v \notin freeVars(value)
```

`select` is semantically equivalent to a particular filter, but it won't always be implemented as a filter.
When the list that we're `select`ing on has an index, `select` will search the index instead of scanning the
list item-by-item. This is where the performance gain will come from- `select`ing on an indexed list will 
have a worst-case time complexity of O(log n) in the size of the list, whereas the item-by-item search will be O(n).
The larger the list, the bigger the performance gain. For a binary-tree index and a list of 1000 elements, the
worst-case indexed search 100x faster than the worse-case linear search (`log_2(1000) ~ 10`).

We can use the cherry-pick rule from the previous section to pick out predicates that look like `?.x == ?` and `? == ?.x`.
Take something like `filter (\x -> p x && x.a == 3) values`, which would be rewritten to 
`filter (\x -> p x) (filter (\x -> x.a == 3) values)`. We can then run the `select` equation backwards on the inner filter
to get `filter (\x -> p x) (select[a] 3 values)`.

## This project

I've implemented the fundamentals of this approach along with some tests and benchmarks. The program transformation 
and index generation phases are amortised over the number of `select`s, and I wanted to see some numbers for
how that works out in practise.

## Misc. improvements

1. The larger the index, the bigger the performance gain. We can perform further program transformations to
   make sure that the `select` on the largest index is always performed first.
2. Only generate the indexes that are required by the program. My demo creates an index for each "column",
   but it's easy to do a final analysis to tally up which indexes are actually required and only build those.
3. Composite indexes. The performance of nested `select`s (i.e. `select[a] x (select [b] y values))`) can be
   be improved by allowing composite `select`s (say, `select[a, b] (x, y) values`). Coupled with the by-need
   index generation from point 2, a composite index can be created to serve this select.