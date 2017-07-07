module SQLExceptions where

import Control.Exception (Exception)

data DuplicateData =
    DuplicateData
    deriving (Eq, Show)

instance Exception DuplicateData
