(*** hide ***)
#r "../packages/NUnit.2.6.2/lib/NUnit.Framework.dll"
#r "../packages/NaturalSpec.1.4.17.1/lib/NaturalSpec.dll"

module Tennis =

    let Love = 0
    let Fifteen = 1
    let Thirty = 2
    let Fourty = 3

    type Player =
    | Player1 
    | Player2
      static member FromInt = function
        | 1 -> Player1
        | _ -> Player2

    type Game = 
    | OpenGame of int * int
    | Deuce
    | Advantage of Player
    | Victory of Player

    let inline (<=>) x y = OpenGame(x,y)

    let NewGame = OpenGame(Love,Love)

    let getScore = function 
    | OpenGame(x,y) -> x,y
    | Deuce -> 4,4
    | Advantage Player1 -> 5,4 
    | Advantage Player2 -> 4,5
    | Victory Player1 -> 5,0
    | Victory Player2 -> 0,5

    let score game player =     
        let oldScore = getScore game
        let newScore =
            match player with
            | Player1 -> fst oldScore + 1,snd oldScore
            | Player2 -> fst oldScore,snd oldScore + 1
        match newScore with
        | x,y when x > Fourty && y > Fourty && x = y -> Deuce
        | x,y when x > Fourty && x-y = 1             -> Advantage Player1
        | x,y when y > Fourty && y-x = 1             -> Advantage Player2
        | x,y when x > Fourty && x > y               -> Victory Player1
        | x,y when y > Fourty && y > x               -> Victory Player2
        | _                                          -> OpenGame newScore

open NaturalSpec
open Tennis

let Player1 = 1
let Player2 = 2

let point_goes_to player game =
    let player = Player.FromInt player
    printMethod player
    score game player

open NUnit.Framework
open System.Collections.Generic

(**
I'm a fan of the [Gherkin DSL](https://github.com/cucumber/cucumber/wiki/Gherkin) for specifying software behaviour. 
Having a structured but human readable language helps the discussion with business users. However, when it comes to 
converting the resulting specifications in executable tests, I find the tooling cumbersome. I suspect I would find 
it more useful if the business users were writing large numbers of tests, but this has never happened on any projects 
that I've been involved with.

I prefer a lighter weight approach where the developers write the tests in plain old code, but hopefully in a format 
that makes it easy to map them back to the specifications. The problem with this is that most convention-based approaches 
or BDD frameworks that I've encountered leave a big gap between the code syntax and the specification syntax. Then I 
discovered [NaturalSpec](http://www.navision-blog.de/blog/2009/02/23/introducing-naturalspec-a-dsl-for-testing-part-i/), 
which leverages F#'s DSL-friendly syntax to produce very readable tests, and I was totally blown away by it. Using it 
you can write test like the following (taken from solution to the [Tennis Kata](http://codingdojo.org/cgi-bin/wiki.pl?KataTennis) 
provided in the NaturalSpec [source code](https://github.com/forki/NaturalSpec)):
*)

[<Scenario>]     
let ``When Player1 scores once the score should be Fifteen to Love`` () =   
    Given NewGame
      |> When point_goes_to Player1
      |> It should equal (Fifteen <=> Love)
      |> Verify

(**
It uses the forward pipe operator to thread the test state through the `When` and `Then` functions, producing very readable code 
with no external state. I wanted to understand how it worked under the hood, but my F# skills weren't up to the task. 
So, to help myself learn I decided to write my own variant at the same time. I'm going to describe that variant, but if you want 
a production-grade solution, please use NaturalSpec!

The general form of a Given/When/Then spec is:

<blockquote><p>
  Given precondition<br />
  When action<br />
  Then expectation
</p></blockquote>

We can convert this into (theoretically) valid F# code by piping each clause into the next:

    Given precondition |> 
    When action |>
    Then expectation

So how do we define the `Given/When/Then` functions such that they compile? First let's deal with the `Given` function:

*)

let Given precondition =
    precondition

(**
This is just some syntactic sugar to make the test more readable; it does nothing but return the precondition. 
We'll need to start looking at function signatures soon, so let's start with this function, which has a signature 
of `'a -> 'a`. We can use it like:
*)

let ``Given example 1`` () = 
    Given (Stack<int>()) 

(*** hide ***)
let ``a binary tree of depth 3`` = ()

(**
or (assuming <code>\`\`a binary tree of depth 3\`\`</code> is already defined)
*)

let ``Given example 2`` () = 
    Given ``a binary tree of depth 3``

(**
Notice how F#'s double-backtick notation makes the code more readable. The `When` function needs to execute 
an action on the precondition (which is piped in from the result of the `Given` clause) and pass on the result 
to the `Then`:
*)

let When action precondition = 
    action precondition

(**
This has the signature `('a -> 'b) -> 'a -> 'b`, and we can then use it in our test like so:
*)

let ``is pushed onto a stack`` n = 
    let s = Stack<int>() 
    s.Push n 
    s 

let ``stack is popped`` (stack: Stack<int>) = 
    let v = stack.Pop() 
    v 

let ``Given/When example`` () = 
    Given (123 |> ``is pushed onto a stack``) |>
    When ``stack is popped`` 

(**
Notice that <code>\`\`stack is popped\`\`</code> takes a `Stack<int>` as its input but returns just the popped value, not 
the entire stack i.e. we do not need to pass the same kind of state between the clauses.

Now we need to assert against the result. The idea here is to use NUnit's asserts rather than writing custom 
ones, but to wrap them in meaningful syntax. So `Then` needs to test the result (which is piped in from the 
result of the `When` clause) against an expectation. Naively, we could assume it takes the form:
*)

let Then expectation actual =
    expectation actual

(**
If we look at the signature, it is `('a -> 'b) -> 'a -> 'b` This looks about right, though we don't expect 
`Then` to return anything meaningful, so would expect `'b` to be `unit`. However, let's try it "as is" before 
messing with the signature. As a first attempt how about trying to make the following syntax compile?
*)

(*** include:basic-example-1 ***)

(**
In other words we are trying to make <code>Then \`\`result is 123\`\` actual_value</code> have the signature 
`('a -> 'b) -> 'a -> 'b`. But because the whole clause is just going to assert, it will return `unit`, and 
then the signature we are gunning for is `('a -> unit) -> 'a -> unit`. So, partially applying 
<code>\`\`result is 123\`\`</code> to `Then` needs to yield a function with signature `'a -> unit`. So 
<code>\`\`result is 123\`\`</code> must have a signature of `'a -> unit`:
*)
let ``result is 123`` (actual: 'a) = 
    Assert.AreEqual(123, actual)

(*** define:basic-example-1 ***)
let ``Basic example 1`` () = 
    Given (123 |> ``is pushed onto a stack``) |> 
    When ``stack is popped`` |>
    Then ``result is 123``

(**
If we try to compile and run this, we get a passing test...and if we change the `Given` to push 999 onto 
the stack, we get the expected failing test. Progress!

However we don't want to hardcode 123 into the expectation; we would prefer to write:
*)

(*** include:basic-example-2 ***)

(**
So now we want to make <code>Then \`\`result is\`\` expected\_value actual\_value</code> have the signature 
`('a -> unit) -> 'a -> unit`. Again, partially applying <code>\`\`result is\`\`</code> to 
`Then` needs to yield a function with signature `'c -> 'c -> unit`
(I've changed the generic letter for clarity) i.e. `'a` must be `('c -> 'c)`. 
So, <code>\`\`result is\`\`</code> must have the signature `('c -> 'c -> unit)`:
*)

let ``result is`` (expected: 'a) (actual: 'a) = 
    Assert.AreEqual(expected, actual)

(*** define:basic-example-2 ***)
let ``Basic example 2`` () = 
    Given (123 |> ``is pushed onto a stack``) |>
    When ``stack is popped`` |>
    Then ``result is`` 123 

(**
If we try to compile and run this, we still get a passing test...and if we change the expected value to 999, 
we still get the expected failing test. More progress!

Can we make the syntax more readable and in line with BDD terminology?
*)

(*** include:basic-example-3 ***)

(**
Comparing this to the previous example, we can imagine that <code>\`\`return\`\`</code> takes the place of 
<code>\`\`result is\`\`</code>, with <code>it</code> and <code>should</code> just being syntactic 
"filler words". i.e. we are looking for signature for <code>it</code> and <code>should</code> such that 
<code>Then \`\`return\`\` 123</code> and <code>Then it should \`\`return\`\` 123</code> have the same 
signature. What function does nothing? How about:
*)

let it f = f

(**
and
*)

let should f = f

(*** hide ***)
let ``return`` (expected: 'a) (actual: 'a) = 
    Assert.AreEqual(expected, actual)
    
(**
They both have signature <code>'a -> 'a</code>. So in the above example, with <code>\`\`return\`\`</code> 
having signature <code>('c -> 'c -> unit)</code>, both <code>should return</code> and 
<code>it should return</code> also have signature <code>('c -> 'c -> unit)</code>. 
This all looks promising...and yes, the code still compiles and we still have the expected passing and 
failing tests.
*)

(*** define:basic-example-3 ***)
let ``Basic example 3`` () = 
    Given (123 |> ``is pushed onto a stack``) |>
    When ``stack is popped`` |>
    Then it should ``return`` 123 

(**
So at this stage the entire implementation (and two test cases) looks like:

    namespace ItWorksOnMyMachine

    open System.Collections.Generic

    open NUnit.Framework

    module FSpec =
        let Given precondition =
            precondition

        let When action precondition = 
            action precondition

        let Then expectation actual =
            expectation actual

        let it f = f

        let should f = f

        let ``return`` (expected: 'a) (actual: 'a) =
            Assert.AreEqual(expected, actual)

    module FSpecExamples =
        open FSpec

        let ``is pushed onto a stack`` n =
            let s = Stack<int>()
            s.Push n
            s
        
        let ``stack is popped`` (stack: Stack<int>) = 
            let v = stack.Pop()
            v

        [<Test>]
        let ``Basic passing example`` () =
            Given (123 |> ``is pushed onto a stack``) |>
            When ``stack is popped`` |>
            Then it should ``return`` 123
       
        [<Test>]
        [<ExpectedException(typeof<AssertionException>)>]
        let ``Basic failing example`` () =
            Given (999 |> ``is pushed onto a stack``) |>
            When ``stack is popped`` |>
            Then it should ``return`` 123

I would recommend comparing the above implementation 
(full solution [here](https://github.com/akashchopra/FSpec)) to the NaturalSpec 
[code](https://github.com/forki/NaturalSpec/blob/master/src/app/NaturalSpec/Syntax.fs). 
Even when you strip away all the nice extra features that NaturalSpec provides, you are still 
left with a very different implementation: one that would never have occurred to me.

Next time I'll look at an implication of those differences, and try extending my implementation to allow 
asserting that expected exceptions are thrown, rather than relying on the ExpectedException attribute.
*)