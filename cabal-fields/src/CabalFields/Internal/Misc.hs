{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators   #-}
module CabalFields.Internal.Misc where

-- | An operator for pair type.
type (:=) a b = (a, b)

-- | A proper operator for pair creation.
--
-- @(,)@ always need parentheses which is often annoying.
--
-- @(':=')@ binds very losely, so you can do:
--
-- >>> 'x' := reverse $ "foo" ++ "bar"
-- ('x',"raboof")
--
-- Or even nested pairs, if you feel LISPy (right associativity is "inherited" from @('$')@):
--
-- >>> 'x' := True := "bar" := ()
-- ('x',(True,("bar",())))
--
-- A common use example is creating maps:
--
-- >>> import qualified Data.Map as Map
-- >>> Map.fromList [ "foo" := True, "bar" := False ]
-- fromList [("bar",False),("foo",True)]
--
pattern (:=) :: a -> b -> a := b
pattern (:=) a b = (a, b)

infixr 0 :=

{-# COMPLETE (:=) #-}
