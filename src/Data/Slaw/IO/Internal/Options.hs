{-|
Module      : Data.Slaw.IO.Internal.Options
Description : Types that represent slaw option maps
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE CPP                        #-}

module Data.Slaw.IO.Internal.Options
  ( StrOrInt(..)
  , WriteYamlOptions(..)
  , PoolCreateOptions(..)
  , ContextOptions(..)
    --
  , kType
  , kSize
  , kMmap
  ) where

import Control.DeepSeq
import Data.Bifunctor
-- import qualified Data.ByteString.Short    as SBS
import qualified Data.ByteString.Lazy     as L
import Data.Default.Class
import Data.Hashable
-- import Data.Int
-- import Data.Maybe
import qualified Data.Text                as T
-- import qualified Data.Vector.Storable     as S
-- import Data.Word
-- import Foreign.Storable
import GHC.Generics (Generic)

import Data.Slaw
import Data.Slaw.Internal
import Data.Slaw.Util

-- Note: the "qzName" argument can consist of multiple names,
-- separated by a pipe character, such as "tag_numbers|tag-numbers".
-- When converting from a record to a map, all of those keys are
-- set to the same value.  When converting from a map to a record
-- the keys are checked in order.  (Therefore, the preferred name
-- should be listed first.)

#define FIELD(qzName, qzField) opt qzName qzField \
  (\qzRec qzVal -> qzRec { qzField = qzVal })

#define NFELD(qzName, qzField, qzPref) optN qzName qzField \
  (\qzRec qzVal -> qzRec { qzField = qzVal }) qzPref

#define CFELD(qzName, qzField, qzTo, qzFrom) opt1 qzName qzField \
  (\qzRec qzVal -> qzRec { qzField = qzVal }) qzTo qzFrom

--

-- | Represents either a string or an integer.
data StrOrInt = StringValue  !T.Text
              | NumericValue !Integer
              deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

instance Default StrOrInt where
  def = NumericValue (-1)

instance Nameable StrOrInt where
  typeName _ =   "StrOrInt"

instance FromSlaw StrOrInt where
  fromSlaw (SlawString    utf8) = (return . StringValue . fromUtf8) utf8
  fromSlaw s@(SlawNumeric _ _ ) = second NumericValue $ fromSlaw s
  fromSlaw s                    = handleOthers s

instance ToSlaw StrOrInt where
  toSlaw (NumericValue n)  = preferNumeric NumInt32 n
  toSlaw (StringValue txt) = SlawString $ toUtf8 txt

--

data WriteYamlOptions = WriteYamlOptions
  { -- | Set it to 'False' if you don't need full fidelity, and
    -- would rather not have all your numbers tagged.
    --
    -- [key]: @tag_numbers@
    --
    -- [type]: boolean
    --
    -- [default]: 'True'
    wyoTagNumbers       :: Maybe Bool
    -- | Set it to 'False' if you don't want to emit the @%YAML@
    -- and @%TAG@ directives.  This makes the tag name more
    -- verbose, and the YAML version unspecified.
    --
    -- [key]: @directives@
    --
    -- [type]: boolean
    --
    -- [default]: 'True'
  , wyoDirectives       :: Maybe Bool
    -- | If 'True', encodes slaw maps as Yaml's @!!omap@.  If
    -- 'False', encodes slaw maps as Yaml's @!!map@ (which means
    -- the ordering might be lost).
    --
    -- [key]: @ordered_maps@
    --
    -- [type]: boolean
    --
    -- [default]: 'True'
  , wyoOrderedMaps      :: Maybe Bool
    -- | If 'True', starts the file with a comment which includes
    -- the g-speak and libYaml version numbers.
    --
    -- [key]: @comment@
    --
    -- [type]: boolean
    --
    -- [default]: 'True' for the file-based functions, but 'False' for the string-based functions
  , wyoComment          :: Maybe Bool
    -- | If not -1, arrays will be truncated after this many
    -- elements are printed.  This makes the file
    -- non-round-trippable.
    --
    -- [key]: @max-array-elements@
    --
    -- [type]: int64
    --
    -- [default]: @-1@
  , wyoMaxArrayElements :: Maybe Integer
    -- | Whether to automatically flush the stream whenever a slaw
    -- is written.  Defaults to only do so if the stream is not
    -- seekable (and therefore likely a pipe, socket, or terminal).
    --
    -- [key]: @auto-flush@
    --
    -- [type]: string - one of @never@, @always@, or @if-not-seekable@.
    --
    -- [default]: @if-not-seekable@
  , wyoAutoFlush        :: Maybe AutoFlush
} deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

instance Default WriteYamlOptions where
  def = WriteYamlOptions
        { wyoTagNumbers       = Nothing
        , wyoDirectives       = Nothing
        , wyoOrderedMaps      = Nothing
        , wyoComment          = Nothing
        , wyoMaxArrayElements = Nothing
        , wyoAutoFlush        = Nothing
        }

instance Nameable WriteYamlOptions where
  typeName _ = "WriteYamlOptions"

instance FromSlaw WriteYamlOptions where
  fromSlaw = Right . recordFromMap writeYamlOptions

instance ToSlaw WriteYamlOptions where
  toSlaw = recordToMapWithFmt writeYamlOptions YamlFile

writeYamlOptions :: Options WriteYamlOptions
writeYamlOptions =
  [ FIELD("tag_numbers|tag-numbers",   wyoTagNumbers      )
  , FIELD("directives",                wyoDirectives      )
  , FIELD("ordered_maps|ordered-maps", wyoOrderedMaps     )
  , FIELD("comment",                   wyoComment         )
  , NFELD("max-array-elements",        wyoMaxArrayElements, NumInt64)
  , FIELD("auto-flush",                wyoAutoFlush       )
  ]

--

data PoolCreateOptions = PoolCreateOptions
  { -- | The pool type.  Theoretically, different types could
    -- exist, but in practice, only the “mmap” type exists.
    --
    -- [key]: @type@
    --
    -- [type]: string
    --
    -- [default]: @mmap@
    pcoType         :: Maybe T.Text
    -- | This enables a newer pool format, which supports a number
    -- of new features, including being able to resize the pool
    -- after it is created.  You should almost always have this
    -- enabled.  The only reason to ever disable it is if you want
    -- to create pools that can be read by very old versions of
    -- Plasma (g-speak < v2.0).
    --
    -- [key]: @resizable@
    --
    -- [type]: boolean
    --
    -- [default]: 'True'
  , pcoResizable    :: Maybe Bool
    -- | This means that the pool will reside in a single file,
    -- rather than creating an entire directory for the pool.
    --
    -- [key]: @single-file@
    --
    -- [type]: boolean
    --
    -- [default]: 'False'
  , pcoSingleFile   :: Maybe Bool
    -- | The size of the pool, in bytes.  This includes the space
    -- used for the header and table of contents, so the actual
    -- amount of space available for proteins will be less.  The
    -- pool wraps around and deletes older proteins once this limit
    -- has been exceeded.
    --
    -- [key]: @size@
    --
    -- [type]: unt64 (bytes)
    --
    -- [default]: (required)
    --
    -- [note]: available to @pool_change_options@
  , pcoSize         :: Maybe Integer
    -- | The size of the table of contents, in entries (one entry
    -- for each protein).  The default value, 0, indicates that
    -- there should be no table of contents.  The table of contents
    -- is an optimization which makes random access of proteins
    -- within the pool faster.  If the number of proteins in the
    -- pool exceeds the table of contents size, then the table of
    -- contents is “decimated” so that it records only ever
    -- other protein, or every 4th protein, etc.
    --
    -- [key]: @toc-capacity@
    --
    -- [type]: unt64 (proteins)
    --
    -- [default]: @0@
  , pcoTocCapacity  :: Maybe Integer
    -- | If 'True', then the pool stops allowing deposits once it
    -- is full.  If 'False', the default, it will wrap around and
    -- delete the oldest proteins as newer ones are deposited.
    --
    -- [key]: @stop-when-full@
    --
    -- [type]: boolean
    --
    -- [default]: 'False'
    --
    -- [note]: available to @pool_change_options@
  , pcoStopWhenFull :: Maybe Bool
    -- | Makes the pool read-only until @frozen@ is set to 'False'
    -- again.
    --
    -- [key]: @frozen@
    --
    -- [type]: boolean
    --
    -- [default]: 'False'
    --
    -- [note]: available to @pool_change_options@
  , pcoFrozen       :: Maybe Bool
    -- | Causes the pool to be automatically deleted once the last
    -- hose to it is closed.  This option can only be set with
    -- @pool_change_options@; it cannot be set when the pool is
    -- created.
    --
    -- [key]: @auto-dispose@
    --
    -- [type]: boolean
    --
    -- [default]: 'False'
    --
    -- [note]: ONLY available to @pool_change_options@
  , pcoAutoDispose  :: Maybe Bool
    -- | Causes the pool to be synced to disk after every deposit.
    -- This should eliminate the possiblity of corruption due to
    -- power failures, at the expense of performance.  Note that on
    -- Linux, the data is only synced to the disk controller, not
    -- to the platter, so there is still a slight risk of data
    -- being lost due to a power failure.
    --
    -- [key]: @sync@
    --
    -- [type]: boolean
    --
    -- [default]: 'False'
    --
    -- [note]: available to @pool_change_options@
  , pcoSync         :: Maybe Bool
    -- | When 'True', Plasma uses @flock()@ for locking operations.
    -- When 'False', Plasma uses System V semaphores.  The default
    -- is to use semaphores on Linux, and @flock()@ on macOS.
    -- Windows always uses Windows mutexes, and is not affected by
    -- this option.
    --
    -- [key]: @flock@
    --
    -- [type]: boolean
    --
    -- [default]: Linux: 'False', macOS: 'True'
  , pcoFlock        :: Maybe Bool
    -- | When 'True', a checksum is computed for each protein, and
    -- stored in the pool.  This should make it easier to detect
    -- corruption, at the expense of some performance.
    --
    -- [key]: @checksum@
    --
    -- [type]: boolean
    --
    -- [default]: 'False'
  , pcoChecksum     :: Maybe Bool
    -- | The UNIX permissions for this pool.  May be specified
    -- either as an int32, or as a string which is parsed as an
    -- octal number.  We recommend only using @7@ or @0@ in each
    -- position, since write permission is needed to read from
    -- pools, and vice versa, so it isn't really possible to
    -- specify read and write permissions separately.  This option
    -- has no effect on Windows.
    --
    -- [key]: @mode@
    --
    -- [type]: string (octal) or int32
    --
    -- [default]: @-1@
  , pcoMode         :: Maybe StrOrInt
    -- | The user which should own this pool.  Maybe be specified
    -- either as a numeric uid, or as a string which is looked up
    -- in the password file.  This option has no effect on Windows.
    --
    -- [key]: @owner@
    --
    -- [type]: string (username) or int32 (uid)
    --
    -- [default]: @-1@
  , pcoOwner        :: Maybe StrOrInt
    -- | The group which should own this pool.  Maybe be specified
    -- either as a numeric gid, or as a string which is looked up
    -- in the group file.  This option has no effect on Windows.
    --
    -- [key]: @group@
    --
    -- [type]: string (groupname) or int32 (gid)
    --
    -- [default]: @-1@
  , pcoGroup        :: Maybe StrOrInt
} deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

instance Default PoolCreateOptions where
  def = PoolCreateOptions
        { pcoType         = Nothing
        , pcoResizable    = Nothing
        , pcoSingleFile   = Nothing
        , pcoSize         = Nothing
        , pcoTocCapacity  = Nothing
        , pcoStopWhenFull = Nothing
        , pcoFrozen       = Nothing
        , pcoAutoDispose  = Nothing
        , pcoSync         = Nothing
        , pcoFlock        = Nothing
        , pcoChecksum     = Nothing
        , pcoMode         = Nothing
        , pcoOwner        = Nothing
        , pcoGroup        = Nothing
        }

instance Nameable PoolCreateOptions where
  typeName _ = "PoolCreateOptions"

instance FromSlaw PoolCreateOptions where
  fromSlaw = Right . recordFromMap poolCreateOptions

instance ToSlaw PoolCreateOptions where
  toSlaw = recordToMap poolCreateOptions

poolCreateOptions :: Options PoolCreateOptions
poolCreateOptions =
  [ FIELD(kType,                         pcoType        )
  , FIELD("resizable",                   pcoResizable   )
  , FIELD("single-file",                 pcoSingleFile  )
  , NFELD(kSize,                         pcoSize        , NumUnt64)
  , NFELD("toc-capacity|index-capacity", pcoTocCapacity , NumUnt64)
  , FIELD("stop-when-full",              pcoStopWhenFull)
  , FIELD("frozen",                      pcoFrozen      )
  , FIELD("auto-dispose",                pcoAutoDispose )
  , FIELD("sync",                        pcoSync        )
  , FIELD("flock",                       pcoFlock       )
  , FIELD("checksum",                    pcoChecksum    )
  , FIELD("mode",                        pcoMode        )
  , FIELD("owner",                       pcoOwner       )
  , FIELD("group",                       pcoGroup       )
  ]

-- | The string “type”.
kType :: T.Text
kType = "type"

-- | The string “size”.
kSize :: T.Text
kSize = "size"

-- | The string “mmap”.
kMmap :: T.Text
kMmap = "mmap"

--

data ContextOptions = ContextOptions
  { coCertificate :: Maybe L.ByteString
  , coPrivateKey  :: Maybe L.ByteString
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

instance Default ContextOptions where
  def = ContextOptions
        { coCertificate = Nothing
        , coPrivateKey  = Nothing
        }

instance Nameable ContextOptions where
  typeName _ = "ContextOptions"

instance FromSlaw ContextOptions where
  fromSlaw = Right . recordFromMap contextOptions

instance ToSlaw ContextOptions where
  toSlaw = recordToMap contextOptions

contextOptions :: Options ContextOptions
contextOptions =
  [ CFELD("certificate", coCertificate, SlawString, ŝm)
  , CFELD("private-key", coPrivateKey,  SlawString, ŝm)
  ]
