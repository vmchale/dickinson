module SyntaxHighlight ( syntaxHighlightFLTK
                       ) where

import qualified Data.Text as T

syntaxHighlightFLTK :: T.Text -> T.Text
syntaxHighlightFLTK = T.map go
    where go '\n' = '\n'
          go _    = 'K'
