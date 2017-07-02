# Minnie

[![Build Status](https://travis-ci.org/jblondin/minnie.svg?branch=master)](https://travis-ci.org/jblondin/minnie)

Minnie is a programming language developed by [Jamie Blondin](https://github.com/jblondin) to
lean various programming language concepts and techniques. It may eventually turn into something, but for now it's just a sandbox.

A REPL can be accessed with `cargo run`:
```
Minnie version 0.0.1
>> let add2 = fn(a) { return a + 2; };
Empty
>> add2(3)
Integer(5)
>> add2
[Expression(Identifier(Identifier { name: "add2" }))]
Function { parameters: [Identifier { name: "a" }], body: [Return(Infix(Add, Identifier(Identifier { name: "a" }), Literal(Int(2))))], context: <hidden> }
```
