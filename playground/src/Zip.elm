module Zip exposing (..)


type Expression
    = Int Int
    | Plus Expression Expression
    | If Expression Expression Expression


type ExpressionContext
    = Assignment2 String
    | Plus1 ExpressionContext Expression
    | Plus2 Expression ExpressionContext
    | If1 ExpressionContext Expression Expression
    | If2 Expression ExpressionContext Expression
    | If3 Expression Expression ExpressionContext


type Assignment
    = Assignment String Expression


type StringContext
    = Assignment1 Expression


type Zipper
    = S String StringContext
    | E Expression ExpressionContext
    | A Assignment


modifyS : (String -> String) -> Zipper -> Zipper
modifyS function zipper =
    case zipper of
        S e c ->
            S (function e) c

        _ ->
            Debug.todo ""


modifyE : (Expression -> Expression) -> Zipper -> Zipper
modifyE function zipper =
    case zipper of
        E e c ->
            E (function e) c

        _ ->
            Debug.todo ""


left : Zipper -> Zipper
left zipper =
    case zipper of
        E e2 (Plus2 e1 c) ->
            E e1 (Plus1 c e2)

        E e2 (If2 e1 c e3) ->
            E e1 (If1 c e2 e3)

        E e3 (If3 e1 e2 c) ->
            E e2 (If2 e1 c e3)

        E e2 (Assignment2 s1) ->
            S s1 (Assignment1 e2)

        _ ->
            Debug.todo ""


right : Zipper -> Zipper
right zipper =
    case zipper of
        S s1 (Assignment1 e2) ->
            E e2 (Assignment2 s1)

        E e1 (Plus1 c e2) ->
            E e2 (Plus2 e1 c)

        E e1 (If1 c e2 e3) ->
            E e2 (If2 e1 c e3)

        E e2 (If2 e1 c e3) ->
            E e3 (If3 e1 e2 c)

        _ ->
            Debug.todo ""


up : Zipper -> Zipper
up zipper =
    case zipper of
        E e1 (Plus1 c e2) ->
            E (Plus e1 e2) c

        E e2 (Plus2 e1 c) ->
            E (Plus e1 e2) c

        E e1 (If1 c e2 e3) ->
            E (If e1 e2 e3) c

        E e2 (If2 e1 c e3) ->
            E (If e1 e2 e3) c

        E e3 (If3 e1 e2 c) ->
            E (If e1 e2 e3) c

        S s1 (Assignment1 e2) ->
            A (Assignment s1 e2)

        _ ->
            Debug.todo ""


down : Zipper -> Zipper
down zipper =
    case zipper of
        E (Plus e1 e2) c ->
            E e1 (Plus1 c e2)

        E (If e1 e2 e3) c ->
            E e1 (If1 c e2 e3)

        A (Assignment s e2) ->
            S s (Assignment1 e2)

        _ ->
            Debug.todo ""



--right : Zipper -> Zipper
--right ( expression, bs ) =
--    case expression of
--        Int _ ->
--            ( expression, bs )
--
--        Plus l r ->
--            ( r, PlusRight l :: bs )
--
--
--up : Zipper -> Zipper
--up ( expression, bs ) =
--    case bs of
--        (PlusLeft r) :: bs_ ->
--            ( Plus expression r, bs_ )
--
--        (PlusRight l) :: bs_ ->
--            ( Plus l expression, bs_ )
--
--        _ ->
--            ( expression, bs )
--
--
--left : Zipper -> Zipper
--left ( expression, bs ) =
--    case expression of
--        Int _ ->
--            ( expression, bs )
--
--        Plus l r ->
--            ( l, PlusLeft r :: bs )


main_ =
    z
        |> down
        |> modifyS String.toUpper
        |> up


z =
    A <|
        Assignment "name"
            (If (Int 4)
                (Int 5)
                (Plus
                    (Plus (Int 1) (Int 2))
                    (Int 3)
                )
            )
