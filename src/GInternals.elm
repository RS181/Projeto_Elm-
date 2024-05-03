module GInternals exposing (..)
{-
Changes made to Doc t 
1) Text String (Maybe t) ==> Text String
  ∴ String can't have a tag associated   

2) Added MkTagged t (Doc t) and MkString String
  ∴ To tag Documents and MKString 
-}
import Debug exposing (toString)

type Doc t
    = Empty
    | Concatenate (() -> Doc t) (() -> Doc t)
    | Nest Int (() -> Doc t)
    | Text String  -- removed (Maybe t) from Text
    | Line String String
    | Union (Doc t) (Doc t)
    | Nesting (Int -> Doc t)
    | Column (Int -> Doc t)
    --! new stuff
    | MkTagged t (Doc t)
    | MkString String


type Normal t
    = NNil
    -- | NText String (() -> Normal t) (Maybe t)
    | NText String (() -> Normal t)
    | NLine Int String (() -> Normal t)
    --! new stuff
    | Ntag t ( () -> Normal t)



-- Internals -------------------------------------------------------------------

-- ! Não vamos utilizar esta função 
updateTag : (String -> Maybe t -> Maybe t) -> Doc t -> Doc t
updateTag updateFn doc =
    case doc of
        Concatenate doc1 doc2 ->
            Concatenate (\() -> updateTag updateFn (doc1 ())) (\() -> updateTag updateFn (doc2 ()))

        Nest i doc1 ->
            Nest i (\() -> updateTag updateFn (doc1 ()))

        Text text  ->
            Text text 

        Union doc1 doc2 ->
            Union (updateTag updateFn doc1) (updateTag updateFn doc2)

        Nesting fn ->
            Nesting (\i -> updateTag updateFn (fn i))

        Column fn ->
            Column (\i -> updateTag updateFn (fn i))

        x ->
            x

--TODO (DÚVIDA) perguntar é necessário colocar os casos para MkString e MkTagged ??
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
                -- Removed maybetag 
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


                --TODO Verificar  definição Ntag !!!
                Ntag tag innerNormal ->
             
                    layoutInner (innerNormal ()) ( toString (tag) :: acc)
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

                -- TODO  Verificar  definição de MkTagged e MkString
                -- NOTA: MkTagged tem definição semelhante  a Nest 
                ( i, MkTagged tag doc ) :: ds ->
                    Ntag tag (\() -> be w k (( i , doc ) :: ds))

                ( i, MkString text ) :: ds ->
                    NText text (\() -> be w (k + String.length text) ds) 
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
            -- Removed tag argument
            NText text innerNormal  ->
                fits (w - String.length text) (innerNormal ())

            NLine _ _ _ ->
                True
            -- TODO Verificar  definição Ntag
            Ntag _ innerNormal ->
                fits w (innerNormal())