module Stdin (read) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (runPure)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.ST (ST, modifySTRef, newSTRef, readSTRef, runST)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(..))
import Node.Buffer (Buffer, toString) as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.Stream (Readable, onClose, onReadable, readEither) as Stream
import Prelude ((<$>), (<>), ($), bind, void)

toString
  :: Maybe (Either String Buffer.Buffer)
  -> String
toString Nothing = "" -- ignore `null`
toString (Just (Left s)) = s
toString (Just (Right b)) =
  (runPure $ unsafeCoerceEff $ Buffer.toString UTF8 b) -- ?

read
  :: forall e
   . Stream.Readable () e
  -> Aff e String
read stdin = makeAff \_ ok -> do
  ref <- unsafeCoerceEff $ newSTRef "" -- ?
  Stream.onClose stdin $ void do
    s <- unsafeCoerceEff $ readSTRef ref -- ?
    ok s
  Stream.onReadable stdin $ void do
    c <- toString <$> Stream.readEither stdin Nothing
    unsafeCoerceEff $ modifySTRef ref \s -> s <> c -- ?
