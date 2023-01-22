module Wilde.Database.SqlJoin
       (
        module Wilde.Database.Sql,

         -- * Simple ER model used by this module

         Entity,
         newEntity,

         Attribute,
         newAttribute,
         newAttribute_singleColumn,
         BasedOn,

         Join,

         -- * Lifting of single table expressions

         liftFieldFromBase,
         liftExprFromBase,
         liftExprInMonad,
         liftMbExprInMonad,

         -- * Monad

         JoinMonad,

         execBasedOn,
         runBasedOn,
         evalBasedOn,

         -- * Operations in the Monad

         includeFromBase,
         includeFromJoined,
         fieldExprs,
         fieldExprList,

         -- * Joining tables/entities

         -- ** Joins via the base table

         joinNatural,
         joinRightOuter,
         joinLeftOuter,
         joinBothOuter,

         -- ** Joins via a joined table

         joinTransNatural,
         joinTransRightOuter,
         joinTransLeftOuter,
         joinTransBothOuter,

         -- * SELECT statement

         selectStatement,

         -- ** \"Simple\" select statement (without GROUP BY)

         SimpleSelect(..),
         simpleSelectWith,
         simpleSelectStatement,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.List.NonEmpty as NonEmpty

import Wilde.Database.Sql

import qualified Data.Map as Map

import Control.Monad.State.Lazy hiding (join)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
{-| A unique table in the database.

Has a name.

[@columnType@]
It identifies the table columns.
It is (probably) an enum type for which each element corresponds to a
column in the table.
Different tables (tables with different names) may be parametrized by the
same type if they have columns of the same names (the column types may be
different, but this is not recommended - if the types differ, use different
@columnType@).
-}
-------------------------------------------------------------------------------
data Entity columnType = Entity { entityTable :: SqlIdentifier }

-- | Constructor of 'Entity'.
newEntity :: SqlIdentifier -> Entity table
newEntity = Entity

instance Functor Entity where
  fmap f (Entity x) = Entity x

-------------------------------------------------------------------------------
{-| One or more columns of an 'Entity' who's content represents a specific
Haskell type.

One or more column of a table/entity.

[@columnType@]
Identifies the table columns.
Also tells (partly) which 'Entity' the 'Attribute' belongs to (since each
value of the type corresponds to a column of the table).  The reason why
the 'Entity' is only \"partly\" determined is that the name of the table
is missing (it is not derived from @columnType@).

[@value@]
Determines which other 'Attribute's this 'Attribute' may be used for
joins with.
The Haskell type that the values of the table columns are transformed to.
Attribute types with the same @value@ type probably have identical
storage structure (number, and type of database columns).
-}
-------------------------------------------------------------------------------
data Attribute columnType value = Attribute (NonEmpty.NonEmpty columnType)

-- | Constructs an 'Attribute'.
newAttribute :: NonEmpty.NonEmpty columnType
             -> Attribute columnType value
newAttribute = Attribute

-- | Constructs an 'Attribute' that has a single column.
newAttribute_singleColumn :: columnType
                          -> Attribute columnType value
newAttribute_singleColumn = Attribute . NonEmpty.singleton

-------------------------------------------------------------------------------
{-| Two purposes: represents (1) a table and (2) a table column.

1. As a table: Represents a table to which it is possible join other tables
via a 'JoinMonad'.
Joins are said to be \"based\" on the first/non-joined table in the
SQL SELECT, which is represented by this type.

2. As a table column: Represents a table column that can be used in a
SQL SELECT statement.  The column either is part of the \"base table\"
(purpose no 1 of this type); or of a table that is joined to
the \"base table\" via a 'JoinMonad'.

Joins are said to be \"based\" on te
[@columnType@]
Represents the single non-joined table/entity.
Columns for this table may be accessed without a join.
-}
-------------------------------------------------------------------------------
data BasedOn columnType = FromBase columnType
                        | FromJoin SqlIdentifier SqlIdentifier  -- (alias,col)
                        | FromJoinAliasedInternal SqlIdentifier -- ^ Used only for internal purposes

instance Functor BasedOn where
  fmap f (FromBase                  x) = FromBase (f x)
  fmap f (FromJoin                a b) = FromJoin a b
  fmap f (FromJoinAliasedInternal   x) = FromJoinAliasedInternal x

instance SQL_IDENTIFIER  a => SQL_IDENTIFIER (BasedOn a) where
  sqlIdentifier (FromBase a) = sqlIdentifier a
  -- FromJoin:s should have been replaced with FromJoinAliasedInternal.
  sqlIdentifier (FromJoin alias colRef) = error "should not exist, should have been replaced by _fieldExpr"
  sqlIdentifier (FromJoinAliasedInternal colName) = colName

_fieldExpr :: SQL_IDENTIFIER table
           => SqlIdentifier -- ^ base table name
           -> BasedOn table -- ^ field
           -> SqlExpr (BasedOn table)
_fieldExpr baseTableName x@(FromBase _)            = tableField baseTableName x
_fieldExpr _             (FromJoin alias colRef)   = tableField alias $ FromJoinAliasedInternal colRef

-- | Monad for performing joins based on the table 'base'.
type JoinMonad base a = State (JoinState base) a

-------------------------------------------------------------------------------
{-| Represents a join of two tables in a SQL SELECT statement.

Via a 'Join', it is possible to select (or otherwise include) a column
from an 'Entity' who's @columnType@ is @other@.

[@baseColummType@]
Identifies the \"base\" table, or a table that already is included in the existing
joins.
(Corresponds to @columnType@ of 'Entity' and 'Attribute'.)

[@otherColummType@]
Identifies the table that should be \"included\" via join.
(Corresponds to @columnType@ of 'Entity' and 'Attribute'.)
-}
-------------------------------------------------------------------------------
-- The qualifier for the other table (either the table name or an alias for it).
newtype Join baseColumnType otherColumnType = Join SqlIdentifier

data JoinState baseColumnType = JoinState
                 {
                   jsBaseEntity :: Entity baseColumnType,
                   jsJoins      :: Joins  baseColumnType
                 }

instance Functor JoinState where
  fmap f (JoinState baseEntity joins) =
    JoinState
    {
      jsBaseEntity = Entity (entityTable baseEntity)
    , jsJoins      = fmap f joins
    }

initialJoinState :: Entity base -> JoinState base
initialJoinState entity =
  JoinState
  {
    jsBaseEntity = entity
  , jsJoins      = joinsEmpty
  }

data Joins base = Joins
                  {
                    joinsAliases :: AliasTracking,
                    joinsListRev :: [JoinSpec base]
                  }

joinsEmpty :: Joins base
joinsEmpty = Joins
             {
               joinsAliases = Map.empty
             , joinsListRev = []
             }

instance Functor Joins where
  fmap f (Joins aliases listRev) =
    Joins
    {
      joinsAliases = aliases
    , joinsListRev = fmap (fmap f) listRev
    }

-- | Complete information about a join.
--
-- \"Other\" table is always the \"right\" table
-- relative the \"outer join type\".
data JoinSpec base
  = JoinBase
    {
      jspcRightTableAndType :: SqlJoinTableAndType
    , jspcColsInBaseTable   :: NonEmpty.NonEmpty base
    , jspcColsInRightTable  :: NonEmpty.NonEmpty SqlIdentifier
    }
  | JoinTrans
    {
      jspcRightTableAndType  :: SqlJoinTableAndType
      -- | "qualifier" for cols in link table.
      -- This is either the name of the link table itself
      -- or an alias for that table.
    , jspcLinkTableQualifier :: SqlIdentifier
    , jspcColsInLinkTable    :: NonEmpty.NonEmpty SqlIdentifier
    , jspcColsInRightTable   :: NonEmpty.NonEmpty SqlIdentifier
    }

instance Functor JoinSpec where
  fmap f ji@(JoinBase { jspcColsInBaseTable = theColsInBaseTable }) =
    ji { jspcColsInBaseTable = newColsInBaseTable }
    where
      newColsInBaseTable = fmap f (jspcColsInBaseTable ji)

-- | Tracks used table aliases.
type AliasTracking = Map.Map SqlIdentifier Int

-- | \"Lifts\" a field from the base table up to the 'BasedOn' table,
-- so that it can be used in a join.
liftFieldFromBase :: table
                  -> BasedOn table
liftFieldFromBase col = FromBase col

-- | \"Lifts\" an 'SqlExpr' on the non-joined table into
-- the 'BasedOn' table,
-- so that it can be used in a join.
liftExprFromBase :: SqlExpr table
                 -> SqlExpr (BasedOn table)
liftExprFromBase = fmap liftFieldFromBase

-------------------------------------------------------------------------------
-- | Lifts an SQL expression on a table to one that operates on a join
-- based on that table.
-------------------------------------------------------------------------------
liftMbExprInMonad :: (SQL_IDENTIFIER table)
                  => Maybe (SqlExpr table)
                  -> JoinMonad table (Maybe (SqlExpr (BasedOn table)))
liftMbExprInMonad mbExpr = pure $ fmap liftExprFromBase mbExpr

liftExprInMonad :: (SQL_IDENTIFIER table)
                => SqlExpr table
                -> JoinMonad table (SqlExpr (BasedOn table))
liftExprInMonad expr = pure $ liftExprFromBase expr

includeFromBase :: Attribute table value
                -> Attribute (BasedOn table) value
includeFromBase (Attribute cols) = Attribute $ fmap FromBase cols

includeFromJoined :: SQL_IDENTIFIER other
                  => Join base other
                  -> Attribute other value
                  -> Attribute (BasedOn base) value
includeFromJoined (Join tableAlias) (Attribute cols) =
  Attribute $ fmap (FromJoin tableAlias . sqlIdentifier) cols

-- | A field expression for each column of the 'Attribute'.
fieldExprList :: SQL_IDENTIFIER table
              => Attribute (BasedOn table) value
              -> JoinMonad table [SqlExpr (BasedOn table)]
fieldExprList = fmap NonEmpty.toList . fieldExprs

-- | A field expression for each column of the 'Attribute'.
fieldExprs :: SQL_IDENTIFIER table
           => Attribute (BasedOn table) value
           -> JoinMonad table (NonEmpty.NonEmpty (SqlExpr (BasedOn table)))
fieldExprs (Attribute cols) =
  do
    joinState <- get
    let baseTableName = entityTable $ jsBaseEntity joinState
    pure $ fmap (_fieldExpr baseTableName) cols


-------------------------------------------------------------------------------
-- - join -
-------------------------------------------------------------------------------


-- | NATURAL join from the base table as left table.
joinNatural :: SQL_IDENTIFIER other
            => Attribute  base  v
            -> (Entity    other
               ,Attribute other v)
            -> JoinMonad base (Join base other)
joinNatural atB (entityO,atO) =
  joinHelperM entityO (newJoinBase outerNothing atB entityO atO)

-- | RIGHT OUTER join from the base table as left table.
joinRightOuter :: SQL_IDENTIFIER other
               => Attribute  base  v
               -> (Entity    other
                  ,Attribute other (Maybe v))
               -> JoinMonad base (Join base other)
joinRightOuter atB (entityO,atO) =
  joinHelperM entityO (newJoinBase outerRight atB entityO atO)

-- | LEFT OUTER join from the base table as left table.
joinLeftOuter :: SQL_IDENTIFIER other
              => Attribute  base  (Maybe v)
              -> (Entity    other
                 ,Attribute other v)
              -> JoinMonad base (Join base other)
joinLeftOuter atB (entityO,atO) =
  joinHelperM entityO (newJoinBase outerLeft atB entityO atO)

-- | BOTH OUTER join from the base table as left table.
joinBothOuter :: SQL_IDENTIFIER other
              => Attribute  base  (Maybe v)
              -> (Entity    other
                 ,Attribute other (Maybe v))
              -> JoinMonad base (Join base other)
joinBothOuter atB (entityO,atO) =
  joinHelperM entityO (newJoinBase outerBoth atB entityO atO)

-- | \"Transitive\" NATURAL join.
joinTransNatural :: (SQL_IDENTIFIER right,
                     SQL_IDENTIFIER link)
                 => (Join base link,
                     Attribute link  v)
                 -> (Entity    right,
                     Attribute right v)
                 -> JoinMonad base (Join base right)
joinTransNatural (joinBaseLink,atLink) (entityR,atR) =
  joinHelperM entityR (newJoinTrans outerNothing joinBaseLink atLink entityR atR)

-- | \"Transitive\" RIGHT OUTER join from the link table as left table.
joinTransRightOuter :: (SQL_IDENTIFIER right,
                        SQL_IDENTIFIER link)
                    => (Join base link,
                        Attribute link  v)
                    -> (Entity    right,
                        Attribute right (Maybe v))
                    -> JoinMonad base (Join base right)
joinTransRightOuter (joinBaseLink,atLink) (entityR,atR) =
  joinHelperM entityR (newJoinTrans outerRight joinBaseLink atLink entityR atR)

-- | \"Transitive\" RIGHT OUTER join from the link table as left table.
joinTransLeftOuter :: (SQL_IDENTIFIER right,
                       SQL_IDENTIFIER link)
                   => (Join base link,
                       Attribute link  (Maybe v))
                   -> (Entity    right,
                       Attribute right v)
                   -> JoinMonad base (Join base right)
joinTransLeftOuter (joinBaseLink,atLink) (entityR,atR) =
  joinHelperM entityR (newJoinTrans outerLeft joinBaseLink atLink entityR atR)

-- | \"Transitive\" RIGHT OUTER join from the link table as left table.
joinTransBothOuter :: (SQL_IDENTIFIER right,
                       SQL_IDENTIFIER link)
                   => (Join base link,
                       Attribute link  (Maybe v))
                   -> (Entity    right,
                       Attribute right (Maybe v))
                   -> JoinMonad base (Join base right)
joinTransBothOuter (joinBaseLink,atLink) (entityR,atR) =
  joinHelperM entityR (newJoinTrans outerBoth joinBaseLink atLink entityR atR)


-------------------------------------------------------------------------------
-- - helpers -
-------------------------------------------------------------------------------


-- | Produces an alias for the given table, and updates the Alias Tracking.
newAlias :: Entity base
         -> AliasTracking
         -> Entity other
         -> (AliasTracking,Maybe SqlIdentifier)
newAlias (Entity baseTableName) aliasMap (Entity otherTableName) =
   case Map.lookup otherTableName aliasMap of
     Nothing ->
       (Map.insert otherTableName 1 aliasMap,
        if baseTableName == otherTableName
        then Just $ otherTableName ++ show 1
        else Nothing
       )
     Just n ->
       (Map.insert otherTableName (n+1) aliasMap,
        Just $ otherTableName ++ show (n+1)
       )

-------------------------------------------------------------------------------
-- | Helper for 'join' - \"monadic\" version of 'joinHelper'.
--
-- Updates the monad state according to the state information received from
-- 'joinHelper'.  pures the 'Join' given from 'joinHelper'.
-------------------------------------------------------------------------------
joinHelperM :: Entity other
            -> (Maybe SqlIdentifier -> JoinSpec base)
            -> JoinMonad base (Join base other)
joinHelperM entity newJspec =
  do
    joinState         <- get
    let (joins',join)  = joinHelper joinState entity newJspec
    modify $ \js -> js { jsJoins = joins' }
    pure join

-------------------------------------------------------------------------------
-- | Helper for 'join'.
--
-- Constructs a new set of joins ('Joins') and the 'Join' to pure from the
-- \"main\" function.
-------------------------------------------------------------------------------
joinHelper :: JoinState base
           -> Entity    other
           -> (Maybe SqlIdentifier -> JoinSpec base)
           -> (Joins    base,
               Join     base other)
joinHelper (JoinState entityBase (Joins aliasTracking joinList))
  entityOther@(Entity otherTable)
  newJspec =
  (joins,join)
  where
    (aliasTracking',mbAlias) = newAlias entityBase aliasTracking entityOther
    join                     = Join $ maybe otherTable id mbAlias
    jspec                    = newJspec mbAlias
    joins                    = Joins aliasTracking' (jspec : joinList)


-- | Helper for constructing 'JoinInfo's, for use with joinHelper:s.
newJoinBase :: SQL_IDENTIFIER other
            => OuterJoinType
            -> Attribute base  vB
            -> Entity    other
            -> Attribute other vO
            -> Maybe SqlIdentifier
            -> JoinSpec base
newJoinBase joinType
  (Attribute baseColumns)
  (Entity    otherTable)
  (Attribute otherColumns)
  mbOtherTableAlias =
  JoinBase
  {
    jspcRightTableAndType =
       SqlJoinTableAndType
       {
         sqljttTable = otherTable
       , sqljttAlias = mbOtherTableAlias
       , sqljttType  = joinType
       }
  , jspcColsInBaseTable  = baseColumns
  , jspcColsInRightTable = fmap sqlIdentifier otherColumns
  }

-- | Helper for constructing 'JoinInfo's, for use with joinHelper:s.
newJoinTrans :: (SQL_IDENTIFIER right,
                 SQL_IDENTIFIER link)
             => OuterJoinType
             -> Join base link
             -> Attribute link  vL
             -> Entity    right
             -> Attribute right vO
             -> Maybe SqlIdentifier
             -> JoinSpec base
newJoinTrans joinType
  (Join      linkTableQualifier)
  (Attribute linkColumns)
  (Entity    rightTable)
  (Attribute rightColumns)
  mbRightTableAlias =
  JoinTrans
  {
    jspcRightTableAndType =
       SqlJoinTableAndType
       {
         sqljttTable = rightTable
       , sqljttAlias = mbRightTableAlias
       , sqljttType  = joinType
       }
  , jspcLinkTableQualifier = linkTableQualifier
  , jspcColsInLinkTable    = fmap sqlIdentifier linkColumns
  , jspcColsInRightTable   = fmap sqlIdentifier rightColumns
  }

-------------------------------------------------------------------------------
-- | Components of a \"simple\" select statement - a statement without group by.
--
-- Usage: Construct values of this type using the record update syntax with the
-- value 'simpleSelectWith'.
-------------------------------------------------------------------------------
data SimpleSelect base =
  SimpleSelect
  {
    simpleSelectExpressions :: [SqlExpr (BasedOn base)]
  , simpleSelectWhere       :: Maybe (SqlExpr (BasedOn base))
  , simpleSelectOrderBy     :: [SqlExpr (BasedOn base)]
  }

-- | An \"empty\" 'SimpleSelect'.
-- Use record update syntax with this value to construct
-- 'SimpleSelect's.
simpleSelectWith :: SimpleSelect base
simpleSelectWith =
  SimpleSelect
  {
    simpleSelectExpressions = []
  , simpleSelectWhere       = Nothing
  , simpleSelectOrderBy     = []
  }

selectStatement :: SQL_IDENTIFIER base
                => [SqlExpr (BasedOn base)]
                -> Maybe (SqlExpr (BasedOn base))
                -> [SqlExpr (BasedOn base)]
                -> JoinMonad base (SqlSelect (BasedOn base))
selectStatement selectExprs whereExpr orderBy =
  do
    joinState <- get
    pure $ joinSqlSelect joinState selectExprs whereExpr orderBy

simpleSelectStatement :: SQL_IDENTIFIER base
                      => SimpleSelect base
                      -> JoinMonad base (SqlSelect (BasedOn base))
simpleSelectStatement (SimpleSelect
                      {
                        simpleSelectExpressions = expressions
                      , simpleSelectWhere       = whereExpression
                      , simpleSelectOrderBy     = orderBy
                      }) =
  do
    joinState <- get
    pure $ joinSqlSelect joinState expressions whereExpression orderBy

joinSqlSelect :: SQL_IDENTIFIER base
              => JoinState base
              -> [SqlExpr (BasedOn base)]
              -> Maybe (SqlExpr (BasedOn base))
              -> [SqlExpr (BasedOn base)]
              -> (SqlSelect (BasedOn base))
joinSqlSelect joinState selectExprs whereExpr orderBy =
   SqlSelect
   baseTableIdent
   joinSqlTableRefs
   selectExprs
   whereExpr
   orderBy
  where
    joinSqlTableRefs = map joinTableFactor $ reverse ((joinsListRev . jsJoins) joinState)
    baseTableIdent = entityTable $ jsBaseEntity joinState
    joinTableFactor :: SQL_IDENTIFIER base
                    => JoinSpec base -> SqlJoin (BasedOn base)
    joinTableFactor (JoinBase {
                        jspcRightTableAndType = rightTableAndType@(SqlJoinTableAndType table mbAlias _),
                        jspcColsInBaseTable   = colsInBase,
                        jspcColsInRightTable  = colsInRight
                        }) = SqlJoin
                             {
                               sqlJoinTableAndType = rightTableAndType
                             , sqlJoinExpr         = expr
                             }
      where
        singleColEqExprList = fmap colsEq $ NonEmpty.zip colsInBase colsInRight
        expr                = foldl1 (binOp andOp) singleColEqExprList
        rightTableQualifier = maybe table id mbAlias
        colsEq (colInBase,colInRight) =
          let
            fieldExprBase = (_fieldExpr baseTableIdent) $ FromBase colInBase
            fieldExprOthr = (_fieldExpr baseTableIdent) $ FromJoin rightTableQualifier colInRight
          in
           eq fieldExprBase fieldExprOthr

    joinTableFactor (JoinTrans {
                        jspcRightTableAndType  = rightTableAndType@(SqlJoinTableAndType table mbAlias _),
                        jspcLinkTableQualifier = linkTableQualifier,
                        jspcColsInLinkTable    = colsInLink,
                        jspcColsInRightTable   = colsInRight
                        }) = SqlJoin
                             {
                               sqlJoinTableAndType = rightTableAndType
                             , sqlJoinExpr         = expr
                             }
      where
        singleColEqExprList = fmap colsEq $ NonEmpty.zip colsInLink colsInRight
        expr                = foldl1 (binOp andOp) singleColEqExprList
        rightTableQualifier = maybe table id mbAlias
        colsEq (colInLink,colInRight) =
          let
            fieldExprLink  = (_fieldExpr baseTableIdent) $ FromJoin linkTableQualifier  colInLink
            fieldExprRight = (_fieldExpr baseTableIdent) $ FromJoin rightTableQualifier colInRight
          in
           eq fieldExprLink fieldExprRight


execBasedOn :: Entity base
            -> JoinMonad base a
            -> JoinState base
execBasedOn entity m = execState m $ initialJoinState entity

runBasedOn :: Entity base
           -> JoinMonad base a
           -> (a,JoinState base)
runBasedOn entity m = runState m $  initialJoinState entity

evalBasedOn :: Entity base
            -> JoinMonad base a
            -> a
evalBasedOn entity m = evalState m $  initialJoinState entity
