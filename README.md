# nox

A simple expression transformer, not inspired by coq.

### quickstart

```console
$ cargo run examples/add.nox
```

### usage

#### expression

expression syntax:

```
<expression> ::= <symbol> | <functor>
<symbol> ::= [a-zA-Z0-9]+
<functor> ::= <symbol> ( [<expression>],* )
```

#### rules and shapes

the two main entities of the languare are `rules` and `shapes`. a rule defines pattern (head) and it's corresponding substitution (body). the rule definition has the following syntax:

```
rule <name:symbol> <head:expression> = <body:expression>
```

here is an example of a rule that swaps elements of a pair:

```
rule swap swap(pair(a, b)) = pair(b, a)
```

shaping is a process of sequential applying of rules to an expression transforming it into a different expression. shaping has the following syntax:

```
shape <expression>
  ... sequence of rule applications ...
done
```

for example here is how you shape expression `swap(pair(f(a), g(b)))` with the `swap` rule defined above:

```
shape swap(pair(f(a), g(b)))
  apply swap
done
```

the result of this shaping is `pair(g(b), f(a))`.

#### anonymous rules

you don't have to define a rule to use it in shaping. you can directly describe it after the `apply` keyword:

```
shape swap(pair(f(a), g(b)))
  apply rule swap(pair(a, b)) = pair(b, a)
done
```
