module Store.SQL.Util.TaggedTypes where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

{-- Tagged-types -------------------------------------------------------

Allows us to extract a 'row' from the database that's just one element

You have no idea how useful this is, so here's a case: building kv-maps from
tables with rows with more than an index and the value.
--}

data TaggedType a = Tag { untag :: a }
   deriving (Eq, Ord, Show)

instance FromField a => FromRow (TaggedType a) where
   fromRow = Tag <$> field

instance ToField a => ToRow (TaggedType a) where
   toRow (Tag v) = [toField v]
