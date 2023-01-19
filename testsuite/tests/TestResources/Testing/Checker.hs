{-# LANGUAGE ScopedTypeVariables #-}
module TestResources.Testing.Checker where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit.Base (Test(..), Assertion,assertFailure)
import Test.HUnit.Lang (FailureReason(..), formatFailureReason)
import Data.List (intersperse)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type Checker a =
    [String] -- ^ descriptions of super components (in reverse order)
    -> a     -- ^ the value to check
    -> Maybe ([String], FailureReason)
    -- ^ component descr (incl super comps) in reversed order,
    -- and failure reason

type PropertyGetter a b = (String, a -> b)

assert :: Checker a -> a -> Assertion
assert checker actual =
    case checker [] actual of
        Nothing -> pure ()
        Just (comps, fr) -> assertFailure (format_componenents comps <> ": " <> formatFailureReason fr)
    where
        format_componenents :: [String] -> String
        format_componenents = foldr (<>) "" . intersperse "->" . reverse

test :: Checker a -> a -> Test
test checker actual = TestCase $ assert checker actual

property :: forall a b. PropertyGetter a b -> Checker b -> Checker a
property (dg, getter) checker = ret_val
    where
        ret_val :: Checker a
        ret_val super_comps actual = checker (dg : super_comps) (getter actual)

-- | Succeeds unconditionally.
anything :: Checker a
anything _ _ = Nothing

eq :: (Show a, Eq a)
  => a -- expected
  -> Checker a
eq expected super_components actual =
    if expected == actual
        then Nothing
        else Just (super_components, ExpectedButGot Nothing (show expected) (show actual))

ne :: (Show a, Eq a)
  => a -- expected
  -> Checker a
ne expected super_components actual =
    if expected /= actual
        then Nothing
        else Just (super_components, ExpectedButGot Nothing ("not " <> show expected) (show actual))
  

-------------------------------------------------------------------------------
-- - Either -
-------------------------------------------------------------------------------


isLeftOf :: Show b => Checker a -> Checker (Either a b)
isLeftOf checkLeft comps (Left x)  = checkLeft ("Left" : comps) x
isLeftOf checkLeft comps (Right x) = Just (comps, ExpectedButGot Nothing "Left" (show (Right (show x) :: Either () String)))

isRightOf :: Show a => Checker b -> Checker (Either a b)
isRightOf checkRight comps (Left x)  = Just (comps, ExpectedButGot Nothing "Right" (show (Left (show x) :: Either String ())))
isRightOf checkRight comps (Right x) = checkRight ("Right" : comps) x
