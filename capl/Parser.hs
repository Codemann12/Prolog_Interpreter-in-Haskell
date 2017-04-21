module Parser
  ( parseProg, parseGoal
  ) where

import Text.Parsec

import Type

-- Try to parse a program
parseProg :: String -> Either String Prog
parseProg = simpleParse prog

-- Try to parse a goal
parseGoal :: String -> Either String Goal
parseGoal = simpleParse goal

-- Parser type
type Parser a = Parsec String [(String, VarIndex)] a

-- Apply a parser to a string
simpleParse :: Parser a -> String -> Either String a
simpleParse p = either (Left . show) Right . runParser p [] ""

-- Parse a goal
goal :: Parser Goal
goal = Goal <$> (list term <* symbol "." <* eof)

-- Parse a program
prog :: Parser Prog
prog = Prog <$> (whitespaces *> many rule <* eof)

-- Parse a rule
rule :: Parser Rule
rule = (:-) <$> term <*> rhs

-- Parse the right hand side of a rule
rhs :: Parser [Term]
rhs = (symbol "." *> return [])
  <|> (symbol ":-" *> list term <* symbol ".")

-- Parse a term
term :: Parser Term
term = var <|> comb

-- Parse a variable term
var :: Parser Term
var = Var <$> do
  name <- varName <* whitespaces
  state <- getState
  let newIdx = do
                 let idx = maximum (-1 : map snd state) + 1
                 updateState ((name, idx) :)
                 return idx
  case name of
    "_" -> newIdx
    _   -> case lookup name state of
      Just idx -> return idx
      Nothing  -> newIdx

-- Parse a variable name
varName :: Parser String
varName = (:) <$> upper <*> many (letter <|> digit <|> char '_')
      <|> (:) <$> char '_' <*> many (letter <|> digit <|> char '_')

-- Parse a combination term
comb :: Parser Term
comb = do
  f <- atom
  args <- maybe [] id <$> optionMaybe (parens (list term))
  whitespaces
  return $ Comb f args

-- Parse an atom
atom :: Parser String
atom = ((:) <$> lower <*> many (letter <|> digit <|> char '_'))
   <|> many1 (oneOf "+-*/<=>'\\:.?@#$&^~")
   <|> many1 digit
   <|> string "[]"

-- Parse a symbol
symbol :: String -> Parser ()
symbol s = string s *> whitespaces

-- Parse a comment
comment :: Parser ()
comment = () <$ (char '%' *> many (noneOf "\n") *> char '\n')

-- Parse whitespaces or a comment
whitespaces :: Parser ()
whitespaces =  skipMany (() <$ space <|> comment)

-- Parse a list separated by commas
list :: Parser a -> Parser [a]
list p = p `sepBy` symbol ","

-- Parse something enclosed in parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

