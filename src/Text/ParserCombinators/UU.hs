-- | The non-exported modules in "Text.ParserCombinators.UU.Demo" contain a list of examples of how to use the main functionality of this library which demonstrates:
--
-- * how to write basic parsers
--
-- * how to to write ambiguous parsers
--
-- * how  error correction works
--
-- * how to fine-tune your parsers to get rid of ambiguities
--
-- * how to use the monadic interface
--
-- * what kind of error messages you can expect if you write erroneous parsers
--
-- * how to use the permutating/merging parsers
--
-- * to see the parsers in action load the module "Text.ParserCombinators.UU.Demo.Examples" or "Text.ParserCombinators.UU.Demo.MergeAndPermute" in @ghci@ and type @show_demos@, while looking at the corresponding code
--

module Text.ParserCombinators.UU ( module Text.ParserCombinators.UU.Core
                                 , module Text.ParserCombinators.UU.Derived
                                 ) where
import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.Derived





