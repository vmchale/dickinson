module Control.Exception.Value ( eitherThrow
                               , eitherThrowIO
                               , maybeThrowIO
                               ) where

import           Control.Exception (Exception, throw, throwIO)

eitherThrow :: Exception e => Either e x -> x
eitherThrow = either throw id

eitherThrowIO :: Exception e => Either e x -> IO x
eitherThrowIO = either throwIO pure

maybeThrowIO :: Exception e => Maybe e -> IO ()
maybeThrowIO (Just e) = throwIO e
maybeThrowIO Nothing  = pure ()
