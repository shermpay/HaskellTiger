module Tiger.Symbol where

import qualified Data.Map as Map

-- | Sym represents a Symbol in the Table
newtype Sym = Sym String deriving (Eq, Ord, Show)

-- | A Table is a SymbolTable that is generally use for mapping Sym to Type
newtype (Show a) => Table a = Table (Map.Map Sym a) deriving (Show)

-- | Add a Sym => Type mapping in a Table
addSym :: (Show a) => Sym -> a -> Table a -> Table a
addSym sym ty (Table tab) =
    Table $ Map.insert sym ty tab
                
-- | Add a String => Type mapping in a ymTable
addString :: (Show a) => String -> a -> Table a -> Table a
addString var = addSym (Sym var)

getSym :: (Show a) => Sym -> Table a -> a
getSym s (Table m) = case Map.lookup s m of
                             Just t -> t
                             Nothing -> error $ "Unable to obtain type for " ++ (show s)

getString :: (Show a) => String -> Table a -> a
getString s = getSym (Sym s)

lookupSym :: (Show a) => Sym -> Table a -> Maybe a
lookupSym s (Table m) = Map.lookup s m

lookupString :: (Show a) => String -> Table a -> Maybe a
lookupString s = lookupSym (Sym s)

-- | Creates an empty Table
emptyTable :: (Show a) => Table a
emptyTable = Table $ Map.empty

-- | Merge two Tables with preference for the first one
mergeTable :: (Show a) => Table a -> Table a -> Table a
mergeTable (Table m1) (Table m2) = Table $ Map.union m1 m2
