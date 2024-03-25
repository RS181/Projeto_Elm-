module Internals exposing (..)


type Doc t
    = Empty
    | Concatenate (() -> Doc t) (() -> Doc t)
    | Nest Int (() -> Doc t)
    | Text String (Maybe t)
    | Line String String
    | Union (Doc t) (Doc t)
    | Nesting (Int -> Doc t)
    | Column (Int -> Doc t)


type Normal t
    = NNil
    | NText String (() -> Normal t) (Maybe t)
    | NLine Int String (() -> Normal t)


-- Internals -------------------------------------------------------------------


updateTag : (String -> Maybe t -> Maybe t) -> Doc t -> Doc t
updateTag updateFn doc =
    case doc of
        Concatenate doc1 doc2 ->
            Concatenate (\() -> updateTag updateFn (doc1 ())) (\() -> updateTag updateFn (doc2 ()))

        Nest i doc1 ->
            Nest i (\() -> updateTag updateFn (doc1 ()))

        Text text maybeTag ->
            Text text (updateFn text maybeTag)

        Union doc1 doc2 ->
            Union (updateTag updateFn doc1) (updateTag updateFn doc2)

        Nesting fn ->
            Nesting (\i -> updateTag updateFn (fn i))

        Column fn ->
            Column (\i -> updateTag updateFn (fn i))

        x ->
            x


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
            Text hsep Nothing

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

                NText text innerNormal maybeTag ->
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

                ( i, Text text maybeTag ) :: ds ->
                    NText text (\() -> be w (k + String.length text) ds) maybeTag

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

            NText text innerNormal _ ->
                fits (w - String.length text) (innerNormal ())

            NLine _ _ _ ->
                True


-- ! new stuff 
type DocV2 tagDoc tagString 
    = EmptyV2
    | ConcatenateV2 (() -> DocV2 tagDoc tagString) (() -> DocV2 tagDoc tagString)
    --Definition of a text (this definition has 2 possibles Tags)
    | TextV2 String  (Maybe tagDoc) (Maybe tagString) 
    | NestV2 Int (() -> DocV2 tagDoc tagString)
    | LineV2 String String
    | UnionV2 (DocV2 tagDoc tagString) (DocV2 tagDoc tagString)
    | NestingV2 (Int -> DocV2 tagDoc tagString)
    | ColumnV2 (Int -> DocV2 tagDoc tagString)





-- todo only updates tagstring (CHECK IF LOGIC IS CORRECT) , basicaly i just 'adjusted' the updateTag
updateStrTagv2 : (String -> Maybe t -> Maybe t) -> DocV2 tagDoc t -> DocV2 tagDoc t
updateStrTagv2 updateFn doc =
    case doc of
        ConcatenateV2 doc1 doc2 ->
            ConcatenateV2 (\() -> updateStrTagv2 updateFn (doc1 ())) (\() -> updateStrTagv2 updateFn (doc2 ()))
        NestV2 i doc1 ->
            NestV2 i (\() -> updateStrTagv2 updateFn (doc1 ()))
        -- todo check this case (the idea is update only the Tagged String's)  
        TextV2 text _ maybeTagString ->
            TextV2 text (Nothing) (updateFn text maybeTagString)
        UnionV2 doc1 doc2 ->
            UnionV2 (updateStrTagv2 updateFn doc1) (updateStrTagv2 updateFn doc2)
        NestingV2 fn ->
            NestingV2 (\i -> updateStrTagv2 updateFn (fn i))
        ColumnV2 fn ->
            ColumnV2 (\i -> updateStrTagv2 updateFn (fn i))
        x ->
            x

-- todo only updates tagDoc (CHECK IF LOGIC IS CORRECT) , basicaly i just 'adjusted' the updateTag
updateDocTagv2 : (String -> Maybe t -> Maybe t) -> DocV2 t tagString -> DocV2 t tagString
updateDocTagv2 updateFn doc =
    case doc of
        ConcatenateV2 doc1 doc2 ->
            ConcatenateV2 (\() -> updateDocTagv2 updateFn (doc1 ())) (\() -> updateDocTagv2 updateFn (doc2 ()))
        NestV2 i doc1 ->
            NestV2 i (\() -> updateDocTagv2 updateFn (doc1 ()))  
        -- todo check this case (the idea is update only the Tagged Doc's)
        TextV2 text maybeDocString _  ->
            TextV2 text (updateFn text maybeDocString) (Nothing) 
        UnionV2 doc1 doc2 ->
            UnionV2 (updateDocTagv2 updateFn doc1) (updateDocTagv2 updateFn doc2)
        NestingV2 fn ->
            NestingV2 (\i -> updateDocTagv2 updateFn (fn i))
        ColumnV2 fn ->
            ColumnV2 (\i -> updateDocTagv2 updateFn (fn i))
        x ->
            x




-- ! end
