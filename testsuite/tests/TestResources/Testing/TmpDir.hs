{-# LANGUAGE ScopedTypeVariables #-}
module TestResources.Testing.TmpDir
(
    -- * dir population
    DirPopulator,

    empty,

    file,
    file_b,
    empty_file,

    dir,
    empty_dir,

    -- * "main" methods

    withTmpFile,
    withTmpFile_b,
    withTmpDir,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified System.IO.Temp as Temp
import qualified System.IO as SysIO
import qualified System.Directory as SysDir

import qualified Data.ByteString as BS


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- ^ An action that populates a directory.
type DirPopulator =
    FilePath -- ^ Path of the tmp dir to populate
    -> IO ()

type FileSystemElementCreator = String -- ^ relative file name (wo dir components)
                               -> DirPopulator

empty :: DirPopulator
empty _ = pure ()

-- ^ Populates a dir with a file.
file :: String -- ^ relative name (without dir components)
     -> String -- ^ contents
     -> DirPopulator
file name contents tmpDirPath =
    SysIO.writeFile (tmpDirPath <> "/" <> name) contents

-- ^ Populates a dir with a file, contents as `BS.ByteString`.
file_b :: String -- ^ relative name (without dir components)
       -> BS.ByteString -- ^ contents
       -> DirPopulator
file_b name contents tmpDirPath = do
    BS.writeFile (tmpDirPath <> "/" <> name) contents

empty_file :: String -- ^ relative name (without dir components)
           -> DirPopulator
empty_file name = file name ""

dir :: String        -- ^ name (without dir components)
    -> DirPopulator  -- ^ dir contents
    -> DirPopulator
dir name dir_contents rootDir = do
    let dirName = rootDir <> "/" <> name
    SysDir.createDirectory dirName
    dir_contents dirName

empty_dir :: String        -- ^ name (without dir components)
          -> DirPopulator
empty_dir name = dir name empty

-- ^ Runs an action with a tmp dir with the contents
-- of a given dir populator.
withTmpDir
    :: forall a.
       DirPopulator
       -- ^ populates the tmp dir
    -> (FilePath -> IO a)
       -- ^ action to run with the path of the tmp dir,
       -- populated by the populator
    -> IO a
withTmpDir populate action =
    Temp.withSystemTempDirectory "wilde" populateAndRunAction
    where
        populateAndRunAction :: FilePath -> IO a
        populateAndRunAction tmpDirPath = do
            populate tmpDirPath
            action   tmpDirPath


-- ^ Runs an action with a tmp dir with the contents
-- of a given dir populator.
withTmpFile
    :: forall a.
       String
       -- ^ file name (without dir components)
    -> String
       -- ^ tmp file contents
    -> (FilePath -> IO a)
       -- ^ action to run with the path of the tmp file
    -> IO a
withTmpFile fileName fileContents action =
    Temp.withSystemTempDirectory "wilde" populateAndRunAction
    where
        populateAndRunAction :: FilePath -> IO a
        populateAndRunAction tmpDirPath = do
            populate tmpDirPath
            action   (tmpDirPath <> "/" <> fileName)

        populate :: DirPopulator
        populate = file fileName fileContents

withTmpFile_b
    :: forall a.
       String
       -- ^ file name (without dir components)
    -> BS.ByteString
       -- ^ tmp file contents
    -> (FilePath -> IO a)
       -- ^ action to run with the path of the tmp file
    -> IO a
withTmpFile_b fileName fileContents action =
    Temp.withSystemTempDirectory "wilde" populateAndRunAction
    where
        populateAndRunAction :: FilePath -> IO a
        populateAndRunAction tmpDirPath = do
            -- DEBUG begin
            -- SysIO.putStrLn ""
            -- SysIO.putStrLn "=============================="
            -- BS.putStr fileContents
            -- SysIO.putStrLn "------------------------------"
            -- DEBUG end
            populate tmpDirPath
            action   (tmpDirPath <> "/" <> fileName)

        populate :: DirPopulator
        populate = file_b fileName fileContents
