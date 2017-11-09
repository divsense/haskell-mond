module ParserT where

import Control.Applicative
import Data.Char

newtype ParserT m a = ParserT { parseT :: String -> m (a, String) }

instance Monad m => Functor (ParserT m) where
    -- fmap :: (a -> b) -> ParserT m a -> ParserT m b
    {-fmap f st = st >>= \x -> return (f x)-}
    fmap f st = do x <- st
                   return $ f x

instance Monad m => Applicative (ParserT m) where
    -- pure :: a -> ParserT m a
    pure a = ParserT $ \inp -> pure (a, inp)

    -- (<*>) :: ParserT m (a -> b) -> Parser m a -> Parser m b
    mf <*> ma = ParseT $ \inp -> do (f, _) <- (parseT mf) inp
                                    (a, inp') <- (parseT ma) inp
                                    return ((f a), inp')

instance Alternative (ParserT m) where
    empty = Parser $ \inp -> pure ([], inp)

    -- (<|>) :: ParserT m a -> ParserT m a -> ParserT m a
    mp <*> mq = ParseT $ \inp -> do (p, inp') <- (parseT mp) inp
                                    case p of
                                        [] -> return ((parseT ma) inp)
                                        otherwise -> return (p, inp')

instance Monad (ParserT m) where
    return = pure

    -- (>>=) :: ParserT m a -> (a -> ParserT m b) -> ParserT m b
    ma >>= f = ParserT $ \inp -> do (a,inp') <- (parseT ma) inp
                                    parseT (f a) $ inp'


item :: Monad m => ParserT m Char
item = Parser $ \inp -> case inp of
                            []     -> return []
                            (x:xs) -> return (x, xs)

sat :: Monad m => (Char -> Bool) -> ParserT m Char
sat p = item >>= \c -> if p c then return c else empty

qt :: Monad m => ParserT m Char
qt = sat (=='\'')

dblqt :: Monad m => ParserT m Char
dblqt = sat (=='"')

slash :: Monad m => ParserT m Char
slash = sat (== '/')

dot :: Monad m => ParserT m Char
dot = sat (== '.')

comma :: Monad m => ParserT m Char
comma = sat (== ',')

colon :: Monad m => ParserT m Char
colon = sat (== ':')

lsbracket :: Monad m => ParserT m Char
lsbracket = sat (== '[')

rsbracket :: Monad m => ParserT m Char
rsbracket = sat (== ']')

digit :: Monad m => ParserT m Char
digit = sat isDigit

alphanum :: Monad m => ParserT m Char
alphanum = sat isAlphaNum

alphanumDash :: Monad m => ParserT m Char
alphanumDash = (sat isAlphaNum) <|> (sat (== '-'))

char :: Monad m => Char -> ParserT m Char
char x = sat (== x)

string :: Monad m => String -> ParserT m String
string [] = return []
string (x:xs) = char x >> string xs >> return (x:xs)

keyword :: Monad m => ParserT m String
keyword = some alphanum

ident :: Monad m => ParserT m String
ident = some alphanumDash

digits :: Monad m => ParserT m String
digits = some digit

operand :: Monad m => ParserT m String
operand = do qt
             x <- some alphanumDash
             qt
             return x
          <|>
          do dblqt
             x <- some alphanumDash
             dblqt
             return x
          <|>
          keyword

nat :: Monad m => ParserT m Int
nat = do xs <- digits
         return (read xs)

flt :: Monad m => ParserT m Float
flt = do a <- some digit 
         sat (== '.')
         b <- some digit
         return (read $ a ++ "." ++ b)

fltnum :: Monad m => ParserT m String
fltnum = do a <- some digit 
            sat (== '.')
            b <- some digit
            return (a ++ "." ++ b)

space :: Monad m => ParserT m ()
space = do many (sat isSpace)
           return ()

spaceSep :: Monad m => ParserT m ()
spaceSep = do some (sat isSpace)
              return ()

token :: Monad m => ParserT m a -> Parser a
token p = do space
             v <- p
             space
             return v

quotedText :: Monad m => ParserT m String
quotedText = singleQuotedText <|> doubleQuotedText

singleQuotedText :: Monad m => ParserT m String
singleQuotedText = do qt
                      xs <- many $ sat (\x -> x /= '\'')
                      qt
                      return xs

doubleQuotedText :: Monad m => ParserT m String
doubleQuotedText = do dblqt
                      xs <- many $ sat (\x -> x /= '"')
                      dblqt
                      return xs

plainText :: Monad m => ParserT m String
plainText = some (sat isPrint)

noSpaceText :: Monad m => ParserT m String
noSpaceText = some $ sat (\x -> x /= ' ' && isPrint x)

