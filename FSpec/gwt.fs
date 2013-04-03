namespace ItWorksOnMyMachine

open System
open System.Collections.Generic

open NUnit.Framework

module FSpec =
    let Given precondition =
        precondition

    type TestContext<'a> =
        | State of 'a
        | Exception of exn

        member this.ToState =
            match this with
            | State(s) -> s
            | Exception(e) -> raise e

        member this.ToException = 
            match this with
            | State(s) -> failwith "No exception raised"
            | Exception(e) -> e

    let When action precondition = 
        try
            State(action precondition)
        with
        | e -> Exception(e)

    let Then expectation actual =
        expectation actual

    let it f = f

    let should f = f

    let be f = f

    let ``return`` (expected: 'a) (context: TestContext<'a>) =
        Assert.AreEqual(expected, context.ToState)

    let throw checkExnType (context: TestContext<'a>) = 
        checkExnType  context.ToException

    let any<'e> ex =
        Assert.IsInstanceOf<'e>(ex)

    let specific<'e> (check: exn -> bool) (ex: exn) =
        Assert.IsInstanceOf<'e>(ex)
        Assert.IsTrue(check ex, "Exception condition not met:\n\n" + ex.ToString())

    let inline (+/-) (expected) (tolerance) (context: TestContext<_>) =
        let zero = LanguagePrimitives.GenericZero
        let actual = context.ToState
        let absDiff = if expected > actual then
                        expected - actual
                      else
                        actual - expected

        let absTolerance = if tolerance < zero then
                                -tolerance
                           else
                                tolerance

        if absDiff < absTolerance then
            ()
        else
            let msg = sprintf "Expected: %s +/- %s\nBut was:  %s" <| expected.ToString() <| tolerance.ToString() <| actual.ToString()
            raise <| AssertionException(msg)

module FSpecExamples =
    open FSpec

    let ``is pushed onto a stack`` n =
        let s = Stack<int>()
        s.Push n
        s
        
    let ``stack is popped`` (stack: Stack<_>) = 
        let v = stack.Pop()
        v

    let inline ``dividing by 3`` (n:^a) : ^a =
        let three:^a = (Seq.init 3 (fun _ -> LanguagePrimitives.GenericOne)) |> Seq.sum
        n / three

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

    [<Test>]
    let ``Basic exception handling 1`` () =   
        Given (Stack<int>()) |>
        When ``stack is popped`` |>
        Then it should throw any<InvalidOperationException>

    [<Test>]
    let ``Basic exception handling 2`` () =   
        Given (Stack<string>()) |>
        When ``stack is popped`` |>
        Then it should throw (specific<InvalidOperationException> (fun e -> e.Message = "Stack empty."))

    [<Test>]
    let ``Comparing floats``() =
        Given 1.0 |>
        When ``dividing by 3`` |>
        Then it should be (0.333333333 +/- 0.00000001)

    [<Test>]
    let ``Comparing decimals``() =
        Given 1.0m |>
        When ``dividing by 3`` |>
        Then it should be (0.333333333m +/- 0.00000001m)