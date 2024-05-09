module GInternals exposing (..)
import Debug exposing (toString)


type Doc t
    = Empty
    | Concatenate (() -> Doc t) (() -> Doc t)
    | Nest Int (() -> Doc t)
    | Text String  
    | Line String String
    | Union (Doc t) (Doc t)
    | Nesting (Int -> Doc t)
    | Column (Int -> Doc t)
    --! new stuff
    | Open t     
    | Close t 


type Normal t
    = NNil
    | NText String (() -> Normal t)
    | NLine Int String (() -> Normal t)
    --! new stuff
    | Nopen t ( () -> Normal t)
    | Nclose t ( () -> Normal t)



-- Internals -------------------------------------------------------------------


{-
Definição do paper de Philip Wadler:
flatten operator replaces each line break (and its associated indentation) by a single space.
-}
flatten : Doc t -> Doc t
flatten doc =
    case doc of
        Concatenate doc1 doc2 ->
            Concatenate (\() -> flatten (doc1 ())) (\() -> flatten (doc2 ()))

        Nest i doc1 ->
            Nest i (\() -> flatten (doc1 ()))

        Union doc1 doc2 ->
            flatten doc1

        Line hsep _ ->
            Text hsep 

        Nesting fn ->
            flatten (fn 0)

        Column fn ->
            flatten (fn 0)
   
        --! DUVIDA : Não me parece ser necessário faltten para Open t  e Close t (CONFIRMAR )
        x ->
            x


layout : Normal t -> String
layout normal =
    let
        layoutInner : Normal t -> List String -> List String
        layoutInner normal2 acc =
            case normal2 of
                NNil ->
                    acc
                NText text innerNormal ->
                    layoutInner (innerNormal ()) (text :: acc)

                NLine i sep innerNormal ->
                    let
                        norm =
                            innerNormal ()
                    in
                    case norm of
                        NLine _ _ _ ->
                            layoutInner (innerNormal ()) (("\n" ++ sep) :: acc)

                        _ ->
                            layoutInner (innerNormal ()) (("\n" ++ copy i " " ++ sep) :: acc)

                --TODO CONFIRMAR DEFINIÇÕES ABAIXO !!

                Nopen _ innerNormal ->
                    layoutInner (innerNormal ()) (acc)

                Nclose _ innerNormal ->
                    layoutInner (innerNormal ()) (acc)

    in
    layoutInner normal []
        |> List.reverse
        |> String.concat


copy : Int -> String -> String
copy i s =
    if i == 0 then
        ""

    else
        s ++ copy (i - 1) s


best : Int -> Int -> Doc t -> Normal t
best width startCol x =
    let
        be : Int -> Int -> List ( Int, Doc t ) -> Normal t
        be w k docs =
            case docs of
                [] ->
                    NNil

                ( i, Empty ) :: ds ->
                    be w k ds

                ( i, Concatenate doc doc2 ) :: ds ->
                    be w k (( i, doc () ) :: ( i, doc2 () ) :: ds)

                ( i, Nest j doc ) :: ds ->
                    be w k (( i + j, doc () ) :: ds)

                ( i, Text text  ) :: ds ->
                    NText text (\() -> be w (k + String.length text) ds) 

                ( i, Line _ vsep ) :: ds ->
                    NLine i vsep (\() -> be w (i + String.length vsep) ds)

                ( i, Union doc doc2 ) :: ds ->
                    better w
                        k
                        (be w k (( i, doc ) :: ds))
                        (\() -> be w k (( i, doc2 ) :: ds))

                ( i, Nesting fn ) :: ds ->
                    be w k (( i, fn i ) :: ds)

                ( i, Column fn ) :: ds ->
                    be w k (( i, fn k ) :: ds)

                --TODO CONFIRMAR DEFINIÇÕES ABAIXO !!
                --TODO definição semelhante a do Text acima (Parece-me bem )

                ( i , Open tag ) :: ds ->
                    Nopen tag (\() -> be w ( i + String.length ( toString(tag) ) )  ds )

                ( i , Close tag ) :: ds ->
                    Nclose tag (\() -> be w ( i + String.length ( toString(tag) ) )  ds )

    in
    be width startCol [ ( 0, x ) ]


better : Int -> Int -> Normal t -> (() -> Normal t) -> Normal t
better w k doc doc2Fn =
    if fits (w - k) doc then
        doc

    else
        doc2Fn ()


fits : Int -> Normal t -> Bool
fits w normal =
    if w < 0 then
        False

    else
        case normal of
            NNil ->
                True
            NText text innerNormal  ->
                fits (w - String.length text) (innerNormal ())
            NLine _ _ _ ->
                True

            --TODO CONFIRMAR DEFINIÇÕES ABAIXO !!
            --Parece-me bem 
            Nopen _ innerNormal ->
                fits w (innerNormal())

            Nclose _ innerNormal ->
                fits w (innerNormal())