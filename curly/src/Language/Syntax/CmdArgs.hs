module Language.Syntax.CmdArgs (
  -- * Exported modules
  module Language.Parser,

  -- * Preprocessing command-line arguments
  OptDescr(..),ArgDescr(..),usageInfo,
  tokenize,
  
  -- * Example usage
  -- $tutorial
 ) where

import qualified Prelude as P
import Language.Parser
import System.Console.GetOpt

-- |Create a Parser that preprocesses the command-line arguments,
-- splitting options and their arguments into a user-defined data
-- type.
tokenize :: Monad m => [OptDescr a] -> (String -> a) -> ParserT [String] m [a]
tokenize options wrap = p^.parserT
  where p a = pure (pure (a,bs))
          where (bs,_,_) = getOpt (ReturnInOrder wrap) options a

instance Functor OptDescr where map = P.fmap
instance Functor ArgDescr where map = P.fmap

{- $tutorial

This module is intended to provide simple parsing functionality to the
handling of command-line arguments. Here is an example of how this module
may be used.


>data Option = Help | Version | Other String
>           deriving Eq
>  
>options = [
>  Option ['h'] ["help"] (NoArg Help) "Display this menu.",
>  Option ['v'] ["version"] (NoArg Version) "Show the version of this program"
>  ]
>
>mainAxiom = single Help >> lift (putStrLn (usageInfo options))
>          <+> single Version >> lift (putStrLn "Version: 1.0")
>
>main = void $ do
>    getArgs >>= (mainAxiom <*< tokenize options Other)

-}


