# Binks, v0.0.0

If you're viewing this, this marks the absolute beginning of the absolute beginning of the implementation of the language syntax and specifics :)

## Basic constructs & layers of linking

So far I have had an idea of a `Node` that can be constructed with `[<>]`. This could denote an allocated space for some(?) data. Nodes can be synonymously called `0-links` and represent the simplest construct from which links can be created. `Node`s belong to the **0th layer** of linking.

The **kth layer** of linking can be constructed by pairing all the elements from the previous **k** layers in a link that has not yet been constructed. This is equivalent to pairing all the elements from the previous **k** layers where one of the two elements needs to belong to the **(k - 1)st layer**.

Thus, the **1st layer** consists of only one link: `[[<>]--[<>]]`. This is called a `00-link` or a `1-link`.

The **2nd layer** contains 3 links:
* `[[<>]--[[<>]--[<>]]]`, also called a `01-link`,
* `[[[<>]--[<>]]--[<>]]`, a `10-link`,
* `[[[<>]--[<>]]--[[<>]--[<>]]]`, a `11-link`.

## Convenient notation & the **3rd layer** of linking

Instead of writing these sequences of characters over and over again, let's create a simple notation:
* `[<>]` is `0`,
* `[[<>]--[<>]]` is `1`,
* Whenever we pair two subexpressions together, we concatenate them and parenthesize the concatenation. The parentheses might be dropped if it's the outermost concatenation,
* `1` can also be written as `00`.

If we rewrite the links from the first 3 layers in the aforementioned notation, we get:
1. `0`
2. `1`
3. `01`, `10`, `11`

Let's now construct the **3rd layer** of linking:
* `0(01)`, `0(10)`, `0(11)`
* `1(01)`, `1(10)`, `1(11)`
* `(01)0`, `(01)1`, `(01)(01)`, `(01)(10)`, `(01)(11)`
* `(10)0`, `(10)1`, `(10)(01)`, `(10)(10)`, `(10)(11)`
* `(11)0`, `(11)1`, `(11)(01)`, `(11)(10)`, `(11)(11)`

It contains 21 elements.

The sequence of lengths is [A001699](https://oeis.org/A001699), coming from the fact that the system of links represents the binary trees, and the layers represent the heights.

## How to verbally pronounce the parenthesized concatenations?

However you want. Maybe one could say ***in*** and ***out*** when referencing the open parenthesis and the close parenthesis respectively?

For example, `(10)(01)` is pronounced *in one zero out in zero one out*.

## What data are contained within the links?

*TBA*

## So, are the links non-associative and non-commutative?

*TBA*
