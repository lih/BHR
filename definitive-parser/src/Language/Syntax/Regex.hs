{-# LANGUAGE TypeFamilies #-}
module Language.Syntax.Regex (regex,runRegex) where

import Language.Parser

runRegex :: String -> String -> [([(String,String)],String)]
runRegex re = join (pure re >*> regex)^..parser <&> map snd

regex :: Parser String (Parser String ([(String,String)],String))
regex = _union
  where _union = (adjacent`sepBy`single '|') <&> sum
        adjacent = many postfixed <&> map concat.sequence
        atom = dot <+> range <+> otherChar <+> between (single '(') (single ')') (named _union)
        named p = cut (option id (map . register<$>between (single '<') (single '>') (many1 (satisfy (/='>'))))) <*> p
          where register n (l,s) = ((n,s):l,s)
        postfixed = comp<$>atom<*>many postfun
          where postfun = satisfy (`elem`"*+?") <&> \c -> case c of
                  '*' -> map sum.many
                  '+' -> map sum.many1
                  '?' -> (+ pure ([],""))
                  _ -> undefined
                comp a fs = compose fs a
        dot = shallow token <$ single '.'
        otherChar = shallow.char<$>noneOf "()[*?|+"
        range = between (single '[') (single ']') t'i'domains
          where t'i'domains = shallow . satisfy <$> (option id (map not <$ single '^')
                                                <*> (many subRange <&> \rs c -> any ($c) rs))
                subRange = mkRange<$>subChar<*single '-' <*>subChar
                           <+> (==)<$>subChar
                mkRange a b = \c -> c>=a && c<=b
                subChar = noneOf "]"
        shallow = (([],).pure<$>)
        char c = c<$single c
