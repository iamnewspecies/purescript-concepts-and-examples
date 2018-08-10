{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Version where

-- import Data.Version (showVersion)
-- import Paths_purescript as Paths

-- #ifndef RELEASE
-- import qualified Development.GitRev as GitRev
-- #endif

versionString :: String
versionString = "0.1.0.0"
--   where
-- #ifdef RELEASE
--   extra = ""
-- #else
--   extra = " [development build; commit: " ++ $(GitRev.gitHash) ++ dirty ++ "]"
--   dirty
--     | $(GitRev.gitDirty) = " DIRTY"
--     | otherwise = ""
-- #endif
