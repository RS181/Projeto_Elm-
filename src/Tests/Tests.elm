module Tests.Tests exposing (..)

import GPretty.GRenderer exposing (..)
import GInternals exposing (..)
import GPretty exposing (Doc,string,append,words,openTag,tagged)
import Debug exposing (toString)
import GPretty exposing (closeTag)





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
exemplo4 = tagged Bold (exemplo1)


exemplo5 : Doc Tag
exemplo5 = tagged Bold  (words ( [exemplo1,exemplo2,exemplo3,exemplo4]))
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
    -- [pretty 30 renderer exemplo4] 
    [pretty 30 renderer exemplo5]
    -- [pretty 50 renderer exemploTag5]



{-
TODO Publicar o pacote elm (tenho um link disto nos marcadores)

Haskelite 

Gostaria de ter:
(mais facil)
mudar unwindstack para para produzir um documento com uma tag que de cor diferente
para a expressão que esta a ser valiada (ver o Prettyprinter)

paper -> ler o inicio não vale a pena ler o paper todo


(mais dificil)

-}