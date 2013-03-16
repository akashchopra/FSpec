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

    let ``is pushed onto a stack`` n =
        let s = Stack<int>()
        s.Push n
        s
        
    let ``stack is popped`` (stack: Stack<int>) = 
        let v = stack.Pop()
        v

    let ``return`` (expected: 'a) (actual: 'a) =
        Assert.AreEqual(expected, actual)

    let it f = f

    let should f = f

    [<Test>]
    let ``Basic passing example`` () =
        Given (123 |> ``is pushed onto a stack``) |>
        When ``stack is popped`` |>
        Then it should ``return`` 123
       
    [<Test>]
    let ``Basic failing example`` () =
        Given (999 |> ``is pushed onto a stack``) |>
        When ``stack is popped`` |>
        Then it should ``return`` 123