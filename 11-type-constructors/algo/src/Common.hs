-- NOTE: we alwys assume programs are α-renamed so no shadowing occurs
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Common where

import Control.Monad (MonadPlus, mzero)
import Language.Perl
import System.IO.Unsafe
import System.ShQQ

type TID = String

type HID = Int

class Eq a =>
      Rewrite a
  {-
   - (==) must be ≡_α
   -}
  where
  type RW a
  (≡) :: a -> a -> Bool
  (≡) = (==)
  {-
   - okay, not exactly an alpha-conversion,
   - but replaces all instances of a variable with another
   -}
  αRename :: TID -> TID -> a -> a
  subst :: RW a -> TID -> a -> a

infix 4 ≡

-- utilities:
(&>>) :: MonadPlus m => Bool -> m a -> m a
(&>>) True = id
(&>>) False = \_ -> mzero

infix 2 &>>

-- when Γ is the first argument
(⊢) :: a -> (a -> b) -> b
aΓ ⊢ f = f aΓ

-- when Γ is the last argument
(|>) :: a -> (a -> b) -> b
aΓ |> f = f aΓ

infixl 2 |>

fresh :: TID -> TID
fresh t =
  let p =
        "my $tid = '" ++
        t ++
        "';" ++
        "$tid =~ /(\\D+)(\\d+)?/;" ++
        "if(defined $2){" ++
        "$_ = $1 . ($2 + 1)" ++ "} else{" ++ "$_ = $1 . '1'" ++ "}"
   in let pp = p ++ "; print"
       in unsafePerformIO $ [sh| perl -e $pp |]

{-
  withPerl $
  eval $
-}
fresh2 :: TID -> TID -> TID
fresh2 t t' =
  let p =
        "my $tid1 = '" ++
        t ++
        "';" ++
        "my $tid2 = '" ++
        t' ++
        "';" ++
        "$tid1 =~ /(\\D+)(\\d+)?/;" ++
        "my ($tid1_1, $tid1_2) = ($1, $2);" ++
        "$tid2 =~ /(\\D+)(\\d+)?/;" ++
        "my ($tid2_1, $tid2_2) = ($1, $2);" ++ "$_ = $tid1_1 . $tid2_1 . '1'"
   in let pp = p ++ "; print"
       in unsafePerformIO $ [sh| perl -e $pp |]

{-
  withPerl $
  eval $
-}
freshfresh :: TID
freshfresh =
  let p = "$_ = 'mpk' . time()"
   in let pp = p ++ "; print"
       in unsafePerformIO $ [sh| perl -e $pp |]
{-
withPerl $ eval $
-}
