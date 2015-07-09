---
layout: default
title:  "Syntax"
section: "???"
---

# Cats Syntax Guide

This project introduces some new syntax and constructions which might
not be familiar to the reader. This document is an overview of certain
patterns you might see in Cats, what they mean, and how they are used.

## Introduction to Kinds

Many of the type classes in Cats are paraterized on a type with the
shape `F[_]`, i.e. a type constructor that takes a type argument, and
produces a valid type. Examples of these types are `Option[_]`,
`List[_]`, and so on. When we talk about the shape of a type
constructor, we are talking about a *kind*.

Here are some kinds:

 * `T`: concrete type (e.g. `Boolean`)
 * `T[_]`: type constructor taking one param (e.g. `Vector[A]`)
 * `T[_, _]`: type constructor taking two params (e.g. `Map[K, V]`)
 * ...and so on.

So we would say that `Monad[F[_]]` means that `Monad` is parameterized
on `F`, a type constructor that takes one parameter. What is monad's
kind? it is a type constructor parameterized on a type constructor,
something that is often referred to as a *higher-kinded type*:

 * `T[_]`: simple type constructor (e.g. `Option[A]`)
 * `T[_[_]]`: higher-kinded unary type constructor (e.g. `Monad[F[_]]`)
 * `T[_[_, _]]`: higher-kinded binary type constructor (e.g. `Arrow[F[_, _]]`)
 * ...and so on.

Anyway, the important thing here is to see that the type parameters we
provide to a trait or method determine the number (and shape) of the
types we must provide, and that we refer to the shape/arity as a
*kind*.

### Currying types

Consider the type `A => B` (i.e. `Function1[A, B]`). This type
provides all the necessary methods to be a valid monad:

 * Given a `B` value, we can define a function that always returns it.
 * Given a `B => C`, we can create an `A => C` (through composition).
 * Given a `B => (A => C)`, we can also create `A => C`.
 * (Also, these methods satisfy the monad laws.)

Notice that in this case, the `B` parameter is the one that varies,
and `A` is fixed. So we want to fix `A` while leaving `B` variable.
Another way to put this is that `Function1` has the kind `F[_, _]` but
we need a type constructor whose kind is `F[_]`. One way we can do
that is via a type alias that fixes the first parameter:

```tut:silent
import cats.Monad

type IntF[B] = Function1[Int, B]

// IntF's kind is F[_], so Monad[IntF] is valid.
implicit object IntFMonad extends Monad[IntF] {
  def pure[B](b: B): Int => B =
    (n: Int) => b

  override def map[B, C](intf: Int => B)(f: B => C): (Int => C) =
    (n: Int) => f(intf(n))

  def flatMap[B, C](intf: Int => B)(f: B => (Int => C)): (Int => C) =
    (n: Int) => f(intf(n))(n)
}
```

This works fine, and as you can see we only have to refer to `IntF`
once, in the `Monad[_]` declaration. But what if we want a monad for
`String => B` instead of `Int => B`? What about `Boolean => B`? Is
there a way to generalize this code to all possible argument types?

Take a moment and try to fill in the ellipses (`...`) in this
otherwise-correct definition:

```scala
import cats.Monad

implicit def f1Monad[A]: Monad[...] =
  new Monad[...] {
    def pure[B](b: B): A => B =
      (a: A) => b

    override def map[B, C](fab: A => B)(f: B => C): A => C =
      (a: A) => f(fab(a))

    def flatMap[B, C](fab: A => B)(f: B => (A => C)): A => C =
      (a: A) => f(fab(a))(a)
  }
```

Hard, isn't it? There's no easy way to provide the first type to
`Function1` without providing the second as well. In the Cats project
we could write this as:

```tut:silent
import cats.Monad

implicit def f1Monad[A]: Monad[A => ?] =
  new Monad[A => ?] {
    def pure[B](b: B): A => B =
      (a: A) => b

    override def map[B, C](fab: A => B)(f: B => C): A => C =
      (a: A) => f(fab(a))

    def flatMap[B, C](fab: A => B)(f: B => (A => C)): A => C =
      f compose fab
      (a: A) => f(fab(a))(a)
  }
```

As you can see, in Cats `A => ?` leaves the second type parameter
"free" while fixing the first to be the concrete type `A`.

### Kind-Projector

... to be continued ...

```tut:silent
import cats.Monad

implicit def f1Monad[A]: Monad[({type L[b] = A => b})#L] =
  new Monad[({type L[b] = A => b})#L] {
    def pure[B](b: B): A => B =
      (a: A) => b

    override def map[B, C](fab: A => B)(f: B => C): A => C =
      (a: A) => f(fab(a))

    def flatMap[B, C](fab: A => B)(f: B => (A => C)): A => C =
      f compose fab
      (a: A) => f(fab(a))(a)
  }
```
