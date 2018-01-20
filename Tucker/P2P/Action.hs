module Tucker.P2P.Action where

import qualified Data.ByteString as BSR

import Tucker.Std
import Tucker.Msg
import Tucker.Enc
import Tucker.Atom
import Tucker.Error
import Tucker.Transport

import Tucker.P2P.Msg
import Tucker.P2P.Node
import Tucker.P2P.Util

-- data CoroAction t = CoroAction { doAction :: MainLoopEnv -> BTCNode -> IO (t, [RouterAction]) }

-- instance Functor CoroAction where
--     f `fmap` (CoroAction c) =
--         CoroAction $ \env node -> c env node >>= \(a, b) -> pure (f a, b)

-- instance Applicative CoroAction where
--     cf <*> c =
--         CoroAction $ \env node -> do
--             (f, a) <- doAction cf env node
--             (v, b) <- doAction c env node
--             return (f v, a ++ b)

--     pure v = return v

-- instance Monad CoroAction where
--     c >>= fc =
--         CoroAction $ \env node -> do
--             (v, a) <- doAction c env node

            

--     return v = CoroAction $ \env node -> return (v, [])
