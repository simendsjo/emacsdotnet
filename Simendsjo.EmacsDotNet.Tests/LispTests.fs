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
    testCase "eval function without parameters without nil argument" <| fun _ ->
      let env = { Env.Env.Empty with functions = Map.add "some-function" (fun _ sexpr -> SExpr.String "ok") Map.empty }
      Expect.equal (Eval.evalOrFail env "(some-function)") ([ SExpr.String "ok" ]) "called without argument"
    testCase "eval function without parameters with nil argument" <| fun _ ->
      let env = { Env.Env.Empty with functions = Map.add "some-function" (fun _ sexpr -> SExpr.String "ok") Map.empty }
      Expect.equal (Eval.evalOrFail env "(some-function nil)") ([ SExpr.String "ok" ]) "called with nil argument"
  ]

[<Tests>]
let fsharpToSExprTests =
  testList "fsharp to SExpr conversions" [
    testCase "simple values" <| fun _ ->
      Expect.equal (fsharpToSExpr (SExpr.Nil)) (SExpr.Nil) "SExpr"
      Expect.equal (fsharpToSExpr ('c' :> obj)) (SExpr.Character 'c') (nameof char)
      Expect.equal (fsharpToSExpr ("string" :> obj)) (SExpr.String "string") (nameof string)

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

[<Tests>]
let sexprToFSharpValues =
  testList "SExpr conversions to dotnet values" [
    testCase "simple values" <| fun _ ->
      Expect.equal (sexprToFSharp (SExpr.Nil) typeof<obj>) (() :> obj) (nameof unit)
      Expect.equal (sexprToFSharp (SExpr.Character 'A') typeof<obj>) ('A' :> obj) (nameof char)
      Expect.equal (sexprToFSharp (SExpr.String "string") typeof<obj>) ("string" :> obj) (nameof string)
      Expect.equal (sexprToFSharp (SExpr.Integer 1) typeof<obj>) (1 :> obj) (nameof int)
      Expect.equal (sexprToFSharp (SExpr.Float 1.0) typeof<obj>) (1.0 :> obj) (nameof int)
      Expect.equal (sexprToFSharp (SExpr.Symbol "t") typeof<obj>) (true :> obj) (nameof int)
      Expect.equal (sexprToFSharp (SExpr.List [SExpr.Integer 1]) typeof<int list>) ([1] :> obj) "hetrogen list"
      Expect.equal (sexprToFSharp (SExpr.List [SExpr.Integer 1; SExpr.Float 1.0]) typeof<obj list>) ([1 :> obj; 1.0 :> obj] :> obj) "homogen list"
    testCase "nil -> bool" <| fun _ ->
      let actual = Lispify.sexprToFSharp SExpr.Nil typeof<bool> :?> bool
      Expect.equal actual false "nil should evaluate to bool"
    testCase "nil -> empty list" <| fun _ ->
      let actual = Lispify.sexprToFSharp SExpr.Nil typeof<obj list> :?> obj list
      Expect.equal actual List.empty<obj> "nil should evaluate to bool"
  ]

module ExposedFunctionTests =
  [<ExposeToEmacsAsTopLevelKebabCase>]
  let nilToNil () = ()

  [<ExposeToEmacsAsTopLevelKebabCase>]
  let manyParameters
    (c : char)
    (b : bool)
    (i : int)
    (f : float)
    (s : string)
    (cs : char list)
    (bs : bool list)
    (is : int list)
    (fs : float list)
    (ss : string list)
    : obj list
    =
      [
        c :> obj
        b :> obj
        i :> obj
        f :> obj
        s :> obj
        cs :> obj
        bs :> obj
        is :> obj
        fs :> obj
        ss :> obj
      ]


  [<Tests>]
  let tests =
    testList "function call tests" [
      testCase "nil to nil" <| fun _ ->
        let env = Env.withFunctionFromQuoted <@ nilToNil @> (Env.Env.Empty)
        let actual = Eval.evalOrFail env "(nil-to-nil nil)"
        Expect.equal actual [SExpr.Nil] "calling function"
      testCase "nil as input can be called without parameters" <| fun _ ->
        let env = Env.withFunctionFromQuoted <@ nilToNil @> (Env.Env.Empty)
        let actual = Eval.evalOrFail env "(nil-to-nil)"
        Expect.equal actual [SExpr.Nil] "calling function"
      testCase "many parameters" <| fun _ ->
        let expected = [
          SExpr.List [
            SExpr.Character 'c'
            SExpr.Symbol "t"
            SExpr.Integer 1
            SExpr.Float 2.0
            SExpr.String "s"
            SExpr.List [ SExpr.Character 'd'; SExpr.Character 'e' ]
            SExpr.List [ SExpr.Symbol "t"; SExpr.Nil ]
            SExpr.List [ SExpr.Integer 1; SExpr.Integer 2 ]
            SExpr.List [ SExpr.Float 3.0; SExpr.Float 4.0 ]
            SExpr.List [ SExpr.String "o"; SExpr.String "k" ]
          ]
        ]
        let env = Env.withFunctionFromQuoted <@ manyParameters @> (Env.Env.Empty)
        let actual = Eval.evalOrFail env "(many-parameters ?c t 1 2.0 \"s\" (?d ?e) (t nil) (1 2) (3.0 4.0) (\"o\" \"k\"))"
        Expect.equal actual expected "calling with many parameters"
    ]