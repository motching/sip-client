module Input where

import Types

sipMessage :: Connection -> IO String
sipMessage = readFile --for now
