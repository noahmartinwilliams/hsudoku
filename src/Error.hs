module Error where

import Data.Either
import Control.Monad.Fail

data Error a = Error (Either String a)

instance Functor Error where
    fmap f (Error (Left str)) = Error (Left str)
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
