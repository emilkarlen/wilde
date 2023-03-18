{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Wilde.ObjectModel.Presentation.FooterRowsConstructor2
(
  FooterConstructor,
  MkFooterConstructor,

  noFooterRows,
  mkNoFooterRows,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.Accumulator as Acc

import           Wilde.GenericUi.AbstractTable (ColGroup)

import           Wilde.WildeUi.StdValueTypes as SVT


import           Wilde.ObjectModel.ObjectModelUtils as OmUtils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------

type FooterConstructor otConf atConf dbTable otNative idAtExisting idAtCreate =
  Acc.Accumulator
    (Object otConf atConf dbTable otNative idAtExisting idAtCreate)
    ([ColGroup WildeStyle],[[WildeCell]])

noFooterRows :: FooterConstructor otConf atConf dbTable otNative idAtExisting idAtCreate
noFooterRows = Acc.constantOf  ([],[])

-- | The first argument is the "column setup", with a (Just 'AttribuetType')
-- for each 'AttributeType' that is displayed.
-- Nothing values represent columns with something else then 'AttributeType's,
-- e.g. buttons.
type MkFooterConstructor otConf atConf dbTable otNative idAtExisting idAtCreate
  =  [Maybe (Any (AttributeType atConf dbTable))]
  -> FooterConstructor otConf atConf dbTable otNative idAtExisting idAtCreate

mkNoFooterRows :: MkFooterConstructor otConf atConf dbTable otNative idAtExisting idAtCreate
mkNoFooterRows = const noFooterRows
