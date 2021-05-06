module Simendjso.EmacsDotNet.Tests.LispTests

open Expecto

open Simendsjo.EmacsDotNet.Lisp


[<Tests>]
let caseChangeTest =
  testList "case changing" [
    testCase "simple test" <| fun _ ->
      let snake = "some_string_here"
      let kebab = "some-string-here"
      let camel = "SomeStringHere"
      let pascal = "someStringHere"
      Expect.equal (toKebabCase kebab) kebab "kebab -> kebab = unchanged"
      Expect.equal (toCamelCase camel) camel "camel -> camel = unchanged"
      Expect.equal (toPascalCase pascal) pascal "pascal -> pascal = unchanged"
      Expect.equal (toKebabCase camel) kebab "camel -> kebab"
      Expect.equal (toKebabCase pascal) kebab "pascal -> kebab"
      Expect.equal (toKebabCase snake) kebab "snake -> kebab"
      Expect.equal (toCamelCase kebab) camel "kebab -> camel"
      Expect.equal (toPascalCase kebab) pascal "kebab -> pascal"
  ]
  
[<Tests>]
let parseReadTests =
    [
      "nil"
      "42"
      "4.2"
      "?A"
      "hei"
      "\"hei\""
      "()"
      "(f x :some-thing 42 :else 4.2 :otherthing ?C \"hei hei\" () nil)"
    ]
    |> List.map (fun orig ->
      testCase orig <| fun _ ->
          let sexpr = Parser.parseOrFail orig
          let printed = sexprToString sexpr
          Expect.equal printed orig "parse -> read equal original"
    )
    |> testList "parse->read tests"

[<Tests>]
let evalTests =
  let evalSingle str = Eval.evalOrFail Env.Env.Empty str |> List.exactlyOne
  testList "eval tests" [
    testCase "eval atoms returns atoms" <| fun _ ->
      Expect.equal (evalSingle "42") (SExpr.Integer 42) "Integer"
      Expect.equal (evalSingle "42.42") (SExpr.Float 42.42) "Float"
      Expect.equal (evalSingle "?A") (SExpr.Character 'A') "Character"
      Expect.equal (evalSingle "\"Hello\"") (SExpr.String "Hello") "String"
    testCase "eval empty list returns nil" <| fun _ ->
      Expect.equal (evalSingle "()") SExpr.Nil "Empty list should evaluate to nil"
    testCase "eval symbol t returns symbol" <| fun _ ->
      Expect.equal (evalSingle "t") (SExpr.Symbol "t") "True is t"
    testCase "eval keyword returns keyword" <| fun _ ->
      Expect.equal (evalSingle ":anything") (SExpr.Symbol ":anything") "Keywords are self-evaluating"
  ]

[<Tests>]
let fsharpToSExprTests =
  testList "fsharp to SExpr conversions" [
    testCase "simple values" <| fun _ ->
      Expect.equal (fsharpToSExpr ((int8 1) :> obj)) (SExpr.Integer 1) (nameof int8)
      Expect.equal (fsharpToSExpr ((int16 1) :> obj)) (SExpr.Integer 1) (nameof int16)
      Expect.equal (fsharpToSExpr ((int32 1) :> obj)) (SExpr.Integer 1) (nameof int32)
      Expect.equal (fsharpToSExpr ((int64 1) :> obj)) (SExpr.Integer 1) (nameof int64)
      Expect.equal (fsharpToSExpr ((uint8 1) :> obj)) (SExpr.Integer 1) (nameof uint8)
      Expect.equal (fsharpToSExpr ((uint16 1) :> obj)) (SExpr.Integer 1) (nameof uint16)
      Expect.equal (fsharpToSExpr ((uint32 1) :> obj)) (SExpr.Integer 1) (nameof uint32)
      Expect.equal (fsharpToSExpr ((uint64 1) :> obj)) (SExpr.Integer 1) (nameof uint64)

      Expect.equal (fsharpToSExpr ((float 1.0) :> obj)) (SExpr.Float 1.0) (nameof float)
      Expect.equal (fsharpToSExpr ((double 1.0) :> obj)) (SExpr.Float 1.0) (nameof double) // same thing as float, but added for completeness
      Expect.equal (fsharpToSExpr ((float32 1.0) :> obj)) (SExpr.Float 1.0) (nameof float32)
      Expect.equal (fsharpToSExpr ((decimal 1.0) :> obj)) (SExpr.Float 1.0) (nameof decimal)
  ]
