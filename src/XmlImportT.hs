module XmlImportT (root, xml) where

import Data.Char
import qualified Data.ByteString.Lazy.Char8 as PS
import Data.Digest.Pure.SHA
import Control.Applicative
import Parser
import Mond

type Element = (NID, Node)
newtype XmlImportT e m a = EitherMondT { runXmlImport :: MEitherT e (MondT m) a }

mkBranch :: NID -> [Element] -> [NID]
mkBranch nid = map fst . filter (\x -> let (_, Node _ pid _ _ _ _) = x in pid == nid)

mkNode :: NID -> NID -> (String,[Prop]) -> String -> [Element] -> String -> [Element]
mkNode nid pid (tag, ps) txt xs etag | tag == etag = ((nid,n):xs)
                                     | otherwise = []
        where n = Node nid pid tag txt ps (mkBranch nid xs)

-- Model
root :: Int -> Parser [Element]
root seed = do many decl
               xs <- xml seed "root"
               let br = mkBranch "root" xs
                   rootElem = ("root", Node "root" "" "root" "" [] br)
               return (rootElem:xs)

xml :: Int -> NID -> Parser [Element]
xml seed pid = (emptyElem nid pid) <|> (fullElem nid pid)
             where nid = (showDigest . sha1 . PS.pack) $ (show seed) ++ pid

decl :: Parser ()
decl = string "<?xml" 
       >> space
       >> (some . sat) (/='?')
       >> string "?>"
       >> space

emptyElem :: NID -> NID -> Parser [Element]
emptyElem nid pid = do lbra
                       x <- name
                       xs <- many attr
                       space
                       string "/>"
                       return [(nid, (Node nid pid x "" xs []))]

fullElem :: NID -> NID -> Parser [Element]
fullElem nid pid = mkNode <$> pure nid
                          <*> pure pid
                          <*> startTag
                          <*> txt
                          <*> branch nid
                          <*> endTag

branch :: NID -> Parser [Element]
branch pid = do space
                x <- element pid
                xs <- branch pid
                space
                return (x ++ xs)
             <|> pure []

element :: NID -> Parser [Element]
element pid = Parser $ \inp -> let nid = (showDigest . sha1 . PS.pack) (pid ++ (show . length) inp)
                                   x = parse (emptyElem nid pid) inp
                               in case x of
                                  Nothing -> parse (fullElem nid pid) inp
                                  Just x' -> Just x'

lbra :: Parser Char
lbra = sat (=='<')

rbra :: Parser Char
rbra = sat (=='>')

startTag :: Parser (String,[Prop])
startTag = do lbra
              x <- name
              xs <- many attr
              rbra
              return (x,xs)

endTag :: Parser String
endTag = do string "</"
            x <- name
            rbra
            return x

attr :: Parser Prop
attr = do space
          x <- name
          sat (=='=')
          y <- attrValue
          space
          return (x,y)

attrValue :: Parser String
attrValue = do qt
               x <- kword
               qt
               return x
            <|>
            do dblqt
               x <- kword
               dblqt
               return x

kword :: Parser String
kword = some $ sat (\x -> x /='<' && x /= '>' && x /= ' ' && x /='/' && x /= '=' && x /= '\'' && x /='"')

name :: Parser String
name = do x <- sat isAlpha
          xs <- kword
          return (x:xs)

txt :: Parser String
txt = do space 
         x <- many $ sat (\x -> x /='<' && x /= '>' && x /= '\n' && x /= '\r')
         space
         return x

