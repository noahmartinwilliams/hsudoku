module Error where

import Data.Either
import Control.Monad.Fail

{- Credit for this goes to ChatGPT which helped me figure out how to make Error a MonadFail.
The conversation can be seen here: https://chatgpt.com/share/684b6edb-2b58-8009-ba0a-88f5cf612dc9 -}

data Error a = Error (Either String a)

instance Functor Error where
    fmap _ (Error (Left str)) = Error (Left str)
    fmap f (Error (Right a)) = Error (Right (f a))

instance Applicative Error where
    pure a = Error (Right a)
    (Error (Left str)) <*> _ = Error (Left str)
    _ <*> (Error (Left str)) = Error (Left str)
    (Error (Right f)) <*> (Error (Right a)) = Error (Right (f a))

instance Monad Error where
    (Error (Left str)) >>= _ = Error (Left str)
    (Error (Right a)) >>= f = (f a)
    a >> b = b

    return a = Error (Right a)

instance MonadFail Error where
    fail str = (Error (Left str))
    

isError :: Error a -> Bool
isError (Error (Left _)) = True
isError _ = False
