module FastParser
       ( Error
       , parseString
       , parseFile
       )
       where

import FastAST

-- | You may change this type to whatever you want - just make sure it
-- is an instance of 'Show'.
type Error = ()

parseString :: String -> Either Error Prog
parseString = undefined

parseFile :: FilePath -> IO (Either Error Prog)
parseFile = undefined
