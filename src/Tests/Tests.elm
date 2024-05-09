module Tests.Tests exposing (..)

import GPretty.GRenderer exposing (..)
import GInternals exposing (..)
import GPretty exposing (Doc,string,append)
import Debug exposing (toString)



--Todo tentar definir um exemplo para isto 

-- Define o tipo tag de negrito
type Tag = Bold
        | Italic 


--Documentos em tag exemplos
exemplo1 : Doc t
exemplo1 = append (string "ola") (string "mundo")





exemplo2 : Doc Tag
exemplo2 = append (Open Bold) <| append (string "ola") <| (Close Bold)

exemplo3 : Doc Tag 
exemplo3 = append (Open Italic) <| append (string "mundo") <| (Close Italic)

exemplo4 : Doc Tag
exemplo4 = append exemplo2 exemplo3

renderer : Renderer Tag String String
renderer =
    { init = ""
    , string = \text acc ->  acc ++ text
    , open = \tag acc -> acc ++ "<"++ toString(tag)  ++ ">"
    , close = \tag acc ->   acc ++  "</" ++toString(tag) ++ ">"
    , untagged = \text acc -> acc ++ text
    , newline = \acc -> acc ++ "\n"
    , outer = \acc -> acc
    }



-- Função que executa os testes

runTests : List String
runTests = 
    -- [pretty 30 renderer exemplo1]
    --[pretty 30 renderer exemplo2] 
    --[pretty 30 renderer exemplo3]
    [pretty 30 renderer exemplo4] 
    -- [pretty 30 renderer exemploTag4]
    -- [pretty 50 renderer exemploTag5]



