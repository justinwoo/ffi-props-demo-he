module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Foreign (Foreign, toForeign)
import Data.Function.Uncurried as FU
import Data.Newtype (class Newtype, unwrap, wrap)
import FFIProps as FP
import Type.Prelude (SProxy(..))

foreign import data HE :: Type
foreign import he
  :: FP.Object
       HE
       ( version :: String
       , encode :: FU.Fn2 String Foreign String
       , decode :: FU.Fn2 String Foreign String
       )

getVersion :: forall e. Eff e String
getVersion = FP.unsafeGetProp (SProxy :: SProxy "version") he

newtype EncodedString = EncodedString String
derive instance newtypeES :: Newtype EncodedString _

type EncodeOptions =
  (
  )

encodeString
  :: forall options options'
   . Union options options' EncodeOptions
  => Record options
  -> String
  -> EncodedString
encodeString options s
  = EncodedString
  $ FU.runFn2 encode s (toForeign options)
  where
    encode
      = unsafePerformEff
      $ FP.unsafeGetProp (SProxy :: SProxy "encode") he

type DecodeOptions =
  (
  )

decodeString
  :: forall options options'
   . Union options options' DecodeOptions
  => Record options
  -> EncodedString
  -> String
decodeString options s
  = FU.runFn2 encode (unwrap s) (toForeign options)
  where
    encode
      = unsafePerformEff
      $ FP.unsafeGetProp (SProxy :: SProxy "decode") he

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  version <- getVersion
  log $ "HE is version " <> version
  let
    encoded = encodeString {} "foo © bar ≠ baz 𝌆 qux"
    decoded = decodeString {} (wrap "foo &copy; bar &ne; baz &#x1D306; qux")
  log $ "encoded " <> unwrap encoded
  log $ "decoded " <> decoded

  -- HE is version 1.1.1
  -- encoded foo &#xA9; bar &#x2260; baz &#x1D306; qux
  -- decoded foo © bar ≠ baz 𝌆 qux
