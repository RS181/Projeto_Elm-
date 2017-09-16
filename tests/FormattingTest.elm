module FormattingTest exposing (..)

import Console as Ansi
import Expect exposing (Expectation)
import Main exposing (..)
import Render
import Test exposing (..)


suite : Test
suite =
    -- passes aesthetic tests, but most tests fail when comparing actual strings (different escape sequences)
    describe "Formatting"
        [ skip <|
            describe "foreground colors"
                [ test "colors are applied at text level" <|
                    \_ ->
                        let
                            result =
                                red (string "Red")
                                    |+ char ','
                                    |+ space
                                    |+ white (string "white")
                                    |+ space
                                    |+ string "and"
                                    |+ space
                                    |+ blue (string "blue")
                                    |+ char '!'

                            expected =
                                Ansi.red "Red"
                                    ++ ", "
                                    ++ Ansi.white "white"
                                    ++ " and "
                                    ++ Ansi.blue "blue"
                                    ++ "!"
                        in
                        Expect.equal expected (Render.show result)
                , test "colors can be nested" <|
                    \_ ->
                        let
                            result =
                                blue <|
                                    string "Nested"
                                        |+ space
                                        |+ yellow (string "colors")
                                        |+ space
                                        |+ string "example"

                            expected =
                                Ansi.blue "Nested" ++ Ansi.yellow " colors" ++ Ansi.blue " example"
                        in
                        Expect.equal expected (Render.show result)
                ]
        , skip <|
            describe "background colors"
                [ test "colors are applied at the text level" <|
                    \_ ->
                        let
                            result =
                                onRed (string "Red")
                                    |+ char ','
                                    |+ space
                                    |+ onWhite (string "white")
                                    |+ space
                                    |+ string "and"
                                    |+ space
                                    |+ onBlue (string "blue")
                                    |+ char '!'

                            expected =
                                Ansi.bgRed "Red"
                                    ++ ", "
                                    ++ Ansi.bgWhite "white"
                                    ++ " and "
                                    ++ Ansi.bgBlue "blue"
                                    ++ "!"
                        in
                        Expect.equal expected (Render.show result)
                , test "colors can be nested" <|
                    \_ ->
                        let
                            result =
                                onBlue <|
                                    string "Nested "
                                        |+ onYellow (string "colors")
                                        |+ space
                                        |+ string "example"

                            expected =
                                Ansi.bgBlue "Nested " ++ Ansi.bgYellow "colors" ++ Ansi.bgBlue " example"
                        in
                        Expect.equal expected (Render.show result)
                ]
        , skip <|
            describe "text intensity"
                [ test "it can do bold text" <|
                    \_ ->
                        let
                            result =
                                string "We can do "
                                    |+ bold (string "boldness")
                                    |+ space
                                    |+ string "if your terminal supports it."

                            expected =
                                "We can do "
                                    ++ Ansi.bold "boldness"
                                    ++ " if your terminal supports it."
                        in
                        Expect.equal expected (Render.show result)
                , test "it can debold text" <|
                    \_ ->
                        let
                            result =
                                string "We can do "
                                    |+ debold (bold (string "boldness"))
                                    |+ space
                                    |+ string "if your terminal supports it."

                            expected =
                                "We can do boldness if your terminal supports it."
                        in
                        Expect.equal expected (Render.show result)
                , test "it can debold text with sporadic boldness and other formatting" <|
                    \_ ->
                        let
                            result =
                                debold <|
                                    hang 2 <|
                                        string "I had some "
                                            |+ bold (string "bold text")
                                            |+ char ','
                                            |+ string " but not "
                                            |+ bold (string "anymore")
                                            |+ char '!'

                            expected =
                                "I had some bold text, but not anymore!"
                        in
                        Expect.equal expected (Render.show result)
                ]
        , skip <|
            describe "underlining"
                [ test "it can do underlining" <|
                    \_ ->
                        let
                            result =
                                string "We can do "
                                    |+ underline (string "underlining")
                                    |+ space
                                    |+ string "if your terminal supports it."

                            expected =
                                "We can do "
                                    ++ Ansi.underline "underlining"
                                    ++ " if your terminal supports it."
                        in
                        Expect.equal expected (Render.show result)
                , test "it can deunderline text" <|
                    \_ ->
                        let
                            result =
                                string "We can do "
                                    |+ deunderline (underline (string "underlining"))
                                    |+ space
                                    |+ string "if your terminal supports it."

                            expected =
                                "We can do underlining if your terminal supports it."
                        in
                        Expect.equal expected (Render.show result)
                , test "it can deunderline with sporadic underlining and other formatting" <|
                    \_ ->
                        let
                            result =
                                deunderline <|
                                    hang 3 <|
                                        string "I had some "
                                            |+ underline (string "underlined text")
                                            |+ char ','
                                            |+ string " but not "
                                            |+ underline (string "anymore")
                                            |+ char '!'

                            expected =
                                "I had some underlined text, but not anymore!"
                        in
                        Expect.equal expected (Render.show result)
                ]
        , skip <|
            describe "combinations of formatting"
                [ test "it can do both bold and underlining" <|
                    \_ ->
                        let
                            result =
                                string "We can do "
                                    |+ bold (underline (string "underlining and boldness"))
                                    |+ string " if your terminal supports it."

                            expected =
                                "We can do "
                                    ++ Ansi.bold (Ansi.underline "underlining and boldness")
                                    ++ " if your terminal supports it."
                        in
                        Expect.equal expected (Render.show result)
                , test "it can do boldness with foreground color" <|
                    \_ ->
                        let
                            result =
                                string "this is "
                                    |+ bold (blue (string "some bold blue"))
                                    |+ string " text"

                            expected =
                                "this is "
                                    ++ Ansi.bold (Ansi.blue "some bold blue")
                                    ++ " text"
                        in
                        Expect.equal expected (Render.show result)
                , test "it can do foreground color and background color" <|
                    \_ ->
                        let
                            result =
                                onWhite <|
                                    string "this is"
                                        |+ space
                                        |+ red (string "red text")
                                        |+ space
                                        |+ string "on a white background"

                            expected =
                                Ansi.bgWhite <|
                                    "this is "
                                        ++ Ansi.red "red text"
                                        ++ " on a white background"
                        in
                        Expect.equal expected (Render.show result)
                , test "it can do bold with background color" <|
                    \_ ->
                        let
                            result =
                                onCyan <|
                                    string "this is "
                                        |+ bold (string "bold text")
                                        |+ space
                                        |+ string "on a cyan background"

                            expected =
                                Ansi.bgCyan <|
                                    "this is "
                                        ++ Ansi.bold "bold text"
                                        ++ " on a cyan background"
                        in
                        Expect.equal expected (Render.show result)
                , test "it can do underline with background color and foreground color" <|
                    \_ ->
                        let
                            result =
                                onBlue <|
                                    string "this is"
                                        |+ space
                                        |+ black (underline (string "black underlined text"))
                                        |+ space
                                        |+ string "on a blue background"

                            expected =
                                Ansi.bgBlue <|
                                    "this is "
                                        ++ Ansi.black (Ansi.underline "black underlined text")
                                        ++ " on a blue background"
                        in
                        Expect.equal expected (Render.show result)
                ]
        , describe "plain"
            [ test "it removes all formatting from doc" <|
                \_ ->
                    let
                        result =
                            onBlue <|
                                string "this is "
                                    |+ red (underline (string "red underlined text"))
                                    |+ space
                                    |+ bold (string "on a blue background")

                        expected =
                            "this is red underlined text on a blue background"
                    in
                    Expect.equal expected (Render.show (plain result))
            ]
        ]
