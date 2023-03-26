module Video.ObjectModel where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.ObjectModel.ObjectModelUtils

import           Wilde.ApplicationConstruction.StandardServices.Tools as SS
import           Wilde.ApplicationConstruction.StandardServices as StandardServices
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup as OLS
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.ReferringObjectsComponent as ReferringObjectsComponent
import           Wilde.ApplicationConstruction.ObjectModel.ObjectType
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.ObjectTypeWithAtDdlInformation as ObjectTypeWithAtDdlInformation
import qualified Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectListFooters as Footers

import           Video.Ui.Style

import qualified Common.ObjectType.AttributeType as AT
import qualified Common.ObjectType.ObjectType as OT

import qualified Video.ObjectModel.Country as Country
import qualified Video.ObjectModel.Genre as Genre
import qualified Video.ObjectModel.SpecificSubject as SpecificSubject
import qualified Video.ObjectModel.Director as Director
import qualified Video.ObjectModel.Title as Title
import qualified Video.ObjectModel.Season as Season
import qualified Video.ObjectModel.Episode as Episode
import qualified Video.ObjectModel.Unit as Unit
import qualified Video.ObjectModel.Recording as Recording


-------------------------------------------------------------------------------
-- - Object Model -
-------------------------------------------------------------------------------


objectModelSetup :: [ObjectTypeWithAtDdlInformation.AnyO ObjectTypeSetup]
objectModelSetup =
  [
    ObjectTypeWithAtDdlInformation.AnyO otsCountry
  , ObjectTypeWithAtDdlInformation.AnyO otsGenre
  , ObjectTypeWithAtDdlInformation.AnyO SpecificSubject.ots
  , ObjectTypeWithAtDdlInformation.AnyO otsDirector
  , ObjectTypeWithAtDdlInformation.AnyO otsTitle
  , ObjectTypeWithAtDdlInformation.AnyO otsSeason
  , ObjectTypeWithAtDdlInformation.AnyO otsEpisode
  , ObjectTypeWithAtDdlInformation.AnyO otsUnit
  , ObjectTypeWithAtDdlInformation.AnyO otsRecording
  ]


-------------------------------------------------------------------------------
-- - country -
-------------------------------------------------------------------------------


otsCountry :: StdAutoPkObjectTypeSetup_ddl Country.Table Country.NativeType
otsCountry = Country.ots'
             `SS.withDependentComponents`
             [ReferringObjectsComponent.optional otsDirector director_country
             ,ReferringObjectsComponent.optional otsTitle    title_country]


-------------------------------------------------------------------------------
-- - genre -
-------------------------------------------------------------------------------


otsGenre :: StdAutoPkObjectTypeSetup_ddl Genre.Table Genre.NativeType
otsGenre = Genre.ots'
           `SS.withDependentComponents`
           [ReferringObjectsComponent.optional otsTitle title_genre]


-------------------------------------------------------------------------------
-- - director -
-------------------------------------------------------------------------------


otsDirector :: StdAutoPkObjectTypeSetup_ddl Director.Table Director.NativeType
otsDirector = (SS.objectTypeSetup otDirector
               (withSingleClassStyle Director.tableName "Director"))
              `SS.withObjectListDisplaySetup`
              (OLS.ObjectListDisplaySetup
               {
                 OLS.displayAts                 = otNonIdAttributeTypes otDirector
               , OLS.orderByInDb                = [Any Director.name]
               , OLS.getMkFooterRowsConstructor = Footers.numberOfObjects
               }
              )
              `SS.withDependentComponents`
              [ReferringObjectsComponent.optional otsTitle title_director]

otDirector :: Director.OObjectType
otDirector = OT.ot_autoPk
     (OT.databaseTable Director.tableName)
     OT.unit_toNative
     (AT.primaryKey_dbAutogen Director.Id)
     [
       Any Director.name
     , Any director_country
     , Any Director.url
     ]

director_country :: StdRefAttributeType_optional_ddl Director.Table
director_country = at_ref_std_optional
                   Director.Country_id
                   otsCountry
                   Country.rps
                   Nothing


-------------------------------------------------------------------------------
-- - title -
-------------------------------------------------------------------------------


otsTitle :: StdAutoPkObjectTypeSetup_ddl Title.Table Title.NativeType
otsTitle = (SS.objectTypeSetup otTitle
            (withSingleClassStyle Title.tableName "Title"))
           `SS.withObjectListDisplaySetup`
           (OLS.ObjectListDisplaySetup
            {
              OLS.displayAts                 = otNonIdAttributeTypes otTitle
            , OLS.orderByInDb                = [Any Title.name]
            , OLS.getMkFooterRowsConstructor = Footers.numberOfObjects
            }
           )
           `SS.withDependentComponents`
           [ ReferringObjectsComponent.optional otsSeason season_title
           , ReferringObjectsComponent.optional otsRecording recording_title
           ]

otTitle :: Title.OObjectType
otTitle = OT.ot_autoPk
          (OT.databaseTable Title.tableName)
          OT.unit_toNative
          (AT.primaryKey_dbAutogen Title.Id)
          [
            Any Title.name
          , Any title_genre
          , Any Title.animated
          , Any Title.status
          , Any title_country
          , Any Title.year
          , Any Title.length
          , Any title_director
          , Any Title.url
          , Any Title.comment
          ]

title_genre :: Title.OptionalReference
title_genre = at_ref_std_optional
              Title.Genre_Id
              otsGenre
              Genre.rps
              Nothing

title_country :: Title.OptionalReference
title_country = at_ref_std_optional
                Title.Country_Id
                otsCountry
                Country.rps
                Nothing

title_director :: Title.OptionalReference
title_director = at_ref_std_optional
                 Title.Director_Id
                 otsDirector
                 Director.rps
                 Nothing


-------------------------------------------------------------------------------
-- - season -
-------------------------------------------------------------------------------


otsSeason :: StdAutoPkObjectTypeSetup_ddl Season.Table Season.NativeType
otsSeason = (SS.objectTypeSetup otSeason
             (withSingleClassStyle Season.tableName "Season"))
            `SS.withObjectListDisplaySetup`
            (OLS.ObjectListDisplaySetup
             {
               OLS.displayAts                 = otNonIdAttributeTypes otSeason
             , OLS.orderByInDb                = [Any Season.name]
             , OLS.getMkFooterRowsConstructor = Footers.numberOfObjects
             }
            )
            `SS.withDependentComponents`
           [ReferringObjectsComponent.optional otsRecording recording_season]

otSeason :: Season.OObjectType
otSeason = OT.ot_autoPk
           (OT.databaseTable Season.tableName)
           OT.unit_toNative
           (AT.primaryKey_dbAutogen Season.Id)
           [ Any Season.name
           , Any Season.number
           , Any Season.status
           , Any Season.year
           , Any season_title
           , Any season_director
           , Any Season.url
           ]

season_title :: StdRefAttributeType_optional_ddl Season.Table
season_title = at_ref_std_optional
               Season.Title_Id
               otsTitle
               Title.rps
               Nothing

season_director :: StdRefAttributeType_optional_ddl Season.Table
season_director = at_ref_std_optional
                  Season.Director_Id
                  otsDirector
                  Director.rps
                  Nothing


-------------------------------------------------------------------------------
-- - episode -
-------------------------------------------------------------------------------


otsEpisode :: StdAutoPkObjectTypeSetup_ddl Episode.Table Episode.NativeType
otsEpisode = (SS.objectTypeSetup otEpisode
             (withSingleClassStyle Episode.tableName "Episode"))
            `SS.withObjectListDisplaySetup`
            (OLS.ObjectListDisplaySetup
             {
               OLS.displayAts                 = otNonIdAttributeTypes otEpisode
             , OLS.orderByInDb                = [Any Episode.name]
             , OLS.getMkFooterRowsConstructor = Footers.numberOfObjects
             }
            )
            `SS.withDependentComponents`
           [ReferringObjectsComponent.optional otsRecording recording_episode]

otEpisode :: Episode.OObjectType
otEpisode = OT.ot_autoPk
            (OT.databaseTable Episode.tableName)
            OT.unit_toNative
            (AT.primaryKey_dbAutogen Episode.Id)
            [ Any Episode.name
            , Any episode_season
            , Any Episode.number
            , Any Episode.status
            , Any episode_director
            , Any Episode.url
            ]

episode_season :: StdRefAttributeType_optional_ddl Episode.Table
episode_season = at_ref_std_optional
               Episode.Season_Id
               otsSeason
               Season.rps
               Nothing

episode_director :: StdRefAttributeType_optional_ddl Episode.Table
episode_director = at_ref_std_optional
                  Episode.Director_Id
                  otsDirector
                  Director.rps
                  Nothing


-------------------------------------------------------------------------------
-- - unit -
-------------------------------------------------------------------------------


otsUnit :: StdAutoPkObjectTypeSetup_ddl Unit.Table Unit.NativeType
otsUnit = Unit.ots'
          `SS.withDependentComponents`
          [ReferringObjectsComponent.optional otsRecording recording_unit]


-------------------------------------------------------------------------------
-- - recording -
-------------------------------------------------------------------------------


otsRecording :: StdAutoPkObjectTypeSetup_ddl Recording.Table Recording.NativeType
otsRecording = (SS.objectTypeSetup otRecording
                (withSingleClassStyle Recording.tableName "Recording"))
               `SS.withObjectListDisplaySetup`
               (OLS.ObjectListDisplaySetup
                {
                  OLS.displayAts                 = otNonIdAttributeTypes otRecording
                , OLS.orderByInDb                = [Any recording_title]
                , OLS.getMkFooterRowsConstructor = Footers.numberOfObjects
                }
               )

otRecording :: Recording.OObjectType
otRecording = OT.ot_autoPk
     (OT.databaseTable Recording.tableName)
     OT.unit_toNative
     (AT.primaryKey_dbAutogen Recording.Id)
     [
       Any recording_unit
     , Any recording_title
     , Any recording_season
     , Any recording_episode
     , Any Recording.rec_size
     ]

recording_unit :: StdRefAttributeType_optional_ddl Recording.Table
recording_unit = at_ref_std_optional
                 Recording.Unit_Id
                 otsUnit
                 Unit.rps
                 Nothing

recording_title :: StdRefAttributeType_optional_ddl Recording.Table
recording_title = at_ref_std_optional
                  Recording.Title_Id
                  otsTitle
                  Title.rps
                  Nothing

recording_season :: StdRefAttributeType_optional_ddl Recording.Table
recording_season = at_ref_std_optional
                   Recording.Season_Id
                   otsSeason
                   Season.rps
                   Nothing

recording_episode :: StdRefAttributeType_optional_ddl Recording.Table
recording_episode = at_ref_std_optional
                    Recording.Episode_Id
                    otsEpisode
                    Episode.rps
                    Nothing
