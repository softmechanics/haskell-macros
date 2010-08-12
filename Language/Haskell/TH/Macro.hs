module Language.Haskell.TH.Macro (macro, macroWithMode) where

import Language.Haskell.TH hiding (clause)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Language.Haskell.Exts.QQ
import Language.Haskell.Exts.Translate
import qualified Language.Haskell.Exts as Hs
import qualified Language.Haskell.Exts.Translate as Hs

-- Simpler quasiquoting, by removing burden of constructing haskell
-- AST.  Useful for very simple transformations, such as the following for
-- hlist:
--   Input:  "hello", (), 5, '0', 3.6
--   Output: "hello" .*. () .*. 5 .*. '0' .*. 3.6 .*. HNil
--
-- This is example is simple enough that generating the AST wouldn't be too hard.
-- Still, it's intuitive to write a macro to:
-- 1) replace "," with ".*."
-- 2) append ".*. HNil"
-- 3) parse as valid haskell

defaultParseMode = Hs.defaultParseMode { Hs.extensions = Hs.knownExtensions }

macroWithMode :: Hs.ParseMode -> (String -> String) -> QuasiQuoter
macroWithMode mode macro 
  = wrapQuoter (splice . macro) $ hsWithMode mode
  where splice s = "$( " ++ s ++ " )" -- generate haskell code instead of TH AST.

macro :: (String -> String) -> QuasiQuoter
macro = macroWithMode defaultParseMode

wrapQuoter :: (String -> String) -> QuasiQuoter -> QuasiQuoter
wrapQuoter f (QuasiQuoter f1 f2) = QuasiQuoter (f1 . f) (f2 . f)


