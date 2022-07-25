# Binks v0.0.1

***NOTICE! This language is in its earliest stages of development, so beware of the ever so frequent breaking changes.***

**Binks** (play on words, lit. *binary links*) is an
1. interpreted -- because the source code is processed by the parser (written in Haskell) and then immediately executed;
2. esoteric -- because there is absolutely no way to write meaningful, optimal real-world software in it;
3. programming language -- because... it's a programming language. *Tautology!*

## Basic Syntax

### ⬖ Creating the nodes and the links

The central data structure in Binks is the **binary tree**. The interior nodes connect two subtrees, but only the leaves can hold the data.
In Binks, the leaves are called *Nodes* or, synonymously, *0-links*, and they represent the simplest construct from which *Links* can be created. *Links* is the Binks' term for interior nodes. A *Link* of two *Nodes* is called a *1-link*. A *link* is either a *Node* or a *Link* (the structure is unspecified).

1. Create the *Node* `a` that holds a value `0`:
   ```
   a [<0>]
   ```
2. Create the *Link* `b` between `a` and the *Node* that holds a value 1 (the order matters):
   ```
   b a [<1>]
   ```
3. Create the *Link* `c` between `a` and `b`:
   ```
   c a b
   ```
4. Create the *Link* `d` of nested structure (spaces matter):
   ```
   d ([<-19>] b) a
   ```
### ⬖ Printing the *link* (REPL)

To print out the contents of the *link*, type `out`, followed by a space, and then provide the identifier of a *link*.

```
binksi> out b
([<0.0>] [<1.0>])
```

### ⬖ Accessing and updating the sublinks

To access or update a link, a non-empty list of directions needs to be specified.
1. `<` means 'enter the left sublink'
2. `>` means 'enter the right sublink'

Appending an asterisk (`*`) after a direction means `keep entering the left (right) sublink indefinitely until we run into a *Node*.

1. Accessing:
   ```
   ? _link _directions
   ```
2. Updating:
   ```
   ! _link _directions _either-an-identifier-or-a-link
   ```

```
binksi> out d
(([<-19.0>] ([<0.0>] [<1.0>])) [<0.0>])
binksi> ? d <>*
[<1.0>]
binksi> ! d > b
binksi> out d
(([<-19.0>] ([<0.0>] [<1.0>])) ([<0.0>] [<1.0>]))
```

Upon reacing a *Node* all the remaining directions are ignored.

### ⬖ Mirroring the link

To mirror a link, type the name of the new link, followed by a space, and then the name of the existing link with the tilde (`~`) prepended to it.

```
binksi> d' ~d
binksi> out d'
(([<1.0>] [<0.0>]) (([<1.0>] [<0.0>]) [<-19.0>]))
```
