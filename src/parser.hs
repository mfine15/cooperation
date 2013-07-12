 module Parser
 where
{--
 import Text.ParserCombinators.Parsec hiding (spaces)
 import System.Environment

 spaces :: Parser ()
 spaces = skipMany1 space

 word = many letter

 parseExpr = do
                char '('
                spaces
                first <- word
                spaces
                rest <- (word <|> char ')')
                if rest == ')'
                then return first
                else return


--}