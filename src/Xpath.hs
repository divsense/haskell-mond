module Xpath where

import Control.Applicative
import Parser

type NodeTest = String
type Axis = String
type Op = String
data Predicate = EqFunc String Op String | EqAttr String Op String
                | Attr String | Tag String | Func String | All deriving Show

data Mode = Abs | Rel deriving Show

data Step = Step Mode Axis NodeTest Predicate deriving Show

slash :: Parser Char
slash = sat (== '/')

dot :: Parser Char
dot = sat (== '.')

colon :: Parser Char
colon = sat (== ':')

lsbracket :: Parser Char
lsbracket = sat (== '[')

rsbracket :: Parser Char
rsbracket = sat (== ']')

path :: Parser [Step]
path = idfunc <|> rpath <|> apath

-- Relative Location Path
rpath :: Parser [Step]
rpath = do p <- step Rel
           s <- (some alstep)
           return (p:s)
        <|>
        do s <- step Rel; return [s]

-- Absolute Location Path
apath :: Parser [Step]
apath = idfunc <|> (many alstep >>= \xs -> let s = Step Abs "root" "" All in return (s:xs))

idfunc :: Parser [Step]
idfunc = do string "id('"
            x <- ident
            string "')"
            xs <- many alstep
            return ((Step Abs "id" x All) : xs)

-- Absolote Location Step
alstep :: Parser Step
alstep = (slash >> slash >> step Abs) <|> (slash >> step Rel)

-- Abbreviations
arpath :: Parser [Step]
arpath = do p <- rpath
            slash
            slash
            return p

-- Step
step :: Mode -> Parser Step
step m = do x <- axis
            n <- ntest
            p <- predicate
            return (Step m x n p)
         <|>
         do x <- axis
            n <- ntest
            return (Step m x n All)
         <|>
         (abstep m)

abstep :: Mode -> Parser Step
abstep m = do dot
              dot
              return $ Step m "parent" "node()" All
           <|> 
           do dot
              return $ Step m "self" "node()" All
           <|>
           do n <- ntest
              p <- predicate
              return (Step m "child" n p)
           <|>
           do n <- ntest
              return (Step m "child" n All)
           <|>
           (sat (== '*') >> return (Step m "child" "node()" All)
       
-- Axis
axis :: Parser String
axis = do x <- axname
          colon
          colon
          return x
       <|> abaxis

axname :: Parser String
axname =     string "ancestor" 
         <|> string "ancestor-or-self"
         <|> string "child"
         <|> string "descendant"
         <|> string "descendant-or-self"
         <|> string "following"
         <|> string "following-sibling"
         <|> string "parent"
         <|> string "preceding"
         <|> string "preceding-sibling"
         <|> string "self"
         <|> string "attribute"

abaxis :: Parser String
abaxis = sat (=='@') >> return "attribute"
            
-- Subset of predicates.
-- Filter node-set
-- [@foo]            : attribute exists
-- [@foo=duck]       : attribute exists and equal to ...
-- [text() = drug]
-- [3]               : position
-- [position() < 3]  : position
-- [last()]

predicate :: Parser Predicate
predicate = do lsbracket
               x <- token pexpr
               rsbracket
               return x

pexpr :: Parser Predicate
pexpr = equality <|> attrp <|> funcp <|> nump <|> tagp

attr :: Parser String
attr = sat (=='@') >> keyword

op :: Parser String
op = (string "=") <|> (string ">") <|> (string ">=") <|> (string "<") <|> (string "<=")

func :: Parser String
func = do x <- keyword
          string "()"
          return x

nump :: Parser Predicate
nump = do x <- digits
          return (EqFunc "position" "=" x)

attrp :: Parser Predicate
attrp = do x <- attr
           return (Attr x)

tagp :: Parser Predicate
tagp = do x <- keyword
          return (Tag x)

funcp :: Parser Predicate
funcp = do x <- func
           return (Func x)

equality :: Parser Predicate
equality = do a <- attr
              x <- token op
              b <- keyword
              return (EqAttr a x b)
           <|>
           do a <- func
              x <- token op
              b <- keyword
              return (EqFunc a x b)

ntest :: Parser String
ntest = do a <- ident
          colon
          b <- ident 
          return (a ++ [':'] ++ b)
        <|>
        ident

