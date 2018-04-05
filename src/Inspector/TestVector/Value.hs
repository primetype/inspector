{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Inspector.TestVector.Value
    ( Value(..)
    , valueParser
    , valueBuilder
    , getValueType
    , Array
    , mkArray
    , arrayParser
    , Object(..)
    , objectParser
    ) where

import           Foundation hiding (String, Array)
import qualified Foundation as F
import           Foundation.Collection (KeyedCollection, Element, IndexedCollection(..))
import qualified Foundation.Collection as F
import           Foundation.Check (Arbitrary(..), Gen, oneof, between, frequency, elements)
import           Foundation.Collection (nonEmpty_)
import           Foundation.String.Read

import           Basement.Bounded

import           Inspector.TestVector.Key (Key, keyParser, keyToString)
import           Inspector.TestVector.Types (Type)
import qualified Inspector.TestVector.Types as Type
import           Inspector.Parser (Parser)
import qualified Inspector.Parser as Parser
import           Inspector.Builder

import           Control.Monad (replicateM, forM_)

-- | represent a value in the Inspector's intermetidate language
--
-- The ideal being to have only one representation to build from/to for
-- every output type
data Value
    = Boolean Bool
    | Integer Integer
    | Floating Double
    | String F.String
    | Array !Array
    | Object !Object
  deriving (Show, Eq, Ord, Typeable)
instance Arbitrary Value where
    arbitrary = genValue defaultDepth

getValueType :: Value -> Type
getValueType (Boolean  _) = Type.Boolean
getValueType (Integer  _) = Type.Signed64
getValueType (Floating _) = Type.Float64
getValueType (String _)   = Type.String
getValueType (Array (ArrayDef [])) = Type.Array (Type.UnsizedArray Type.Boolean)
getValueType (Array (ArrayDef (x:_))) = Type.Array (Type.UnsizedArray (getValueType x))
getValueType (Object (ObjectDef lv))  = Type.Object (Type.ObjectDef (fmap (\(k, v) -> (k, getValueType v)) lv))

-- | test the given Values are of the same type
valueEq :: Value -> Value -> Bool
valueEq (Boolean  _) (Boolean  _) = True
valueEq (Integer  _) (Integer  _) = True
valueEq (Floating _) (Floating _) = True
valueEq (String   _) (String   _) = True
valueEq (Array    l) (Array    r) = arrayEq l r
valueEq (Object   l) (Object   r) = objectEq l r
valueEq _            _            = False

-- | represent a collection of value
--
-- the only contraint is that the value must be of the same type.
--
newtype Array = ArrayDef [Value]
  deriving (Show, Eq, Ord, Typeable, Semigroup, Monoid, Collection, Sequential, IndexedCollection, Foldable)
type instance Element Array = Value
instance IsList Array where
    type Item Array = Value
    toList (ArrayDef l) = l
    fromList = fromMaybe (error "Invalid Array") . mkArray
instance Arbitrary Array where
    arbitrary = genArray defaultDepth

-- | smart constructor, check the value are of the same type before allowing
-- constructing it.
mkArray :: [Value] -> Maybe Array
mkArray [] = Just $ ArrayDef []
mkArray [a] = Just $ ArrayDef [a]
mkArray (x:xs) = if and $ fmap (valueEq x) xs
    then Just $ ArrayDef $ x : xs
    else Nothing

-- | test the given Arrays are of the same type
arrayEq :: Array -> Array -> Bool
arrayEq (ArrayDef l) (ArrayDef r) = and $ fmap (uncurry valueEq) $ zip l r

newtype Object = ObjectDef [(Key, Value)]
  deriving (Show, Eq, Ord, Typeable, Semigroup, Monoid, Collection, Sequential, IndexedCollection, Foldable)
instance Arbitrary Object where
    arbitrary = genObject defaultDepth

type instance Element Object = (Key, Value)

instance KeyedCollection Object where
    type Key Object   = Key
    type Value Object = Value
    lookup k = F.lookup k . toList

instance IsList Object where
    type Item Object = (Key, Value)
    toList (ObjectDef l) = l
    fromList = ObjectDef

-- | test the given Objects are of the same type
objectEq :: Object -> Object -> Bool
objectEq (ObjectDef l) (ObjectDef r) = and $ fmap test $ zip l r
  where
    test ((kl, vl), (kr, vr)) = kl == kr && valueEq vl vr

-- Helpers --------------------------------------------------------------------

zip :: [a] -> [b] -> [(a,b)]
zip = F.zip
{-# INLINE zip #-}

-- Builder --------------------------------------------------------------------

valueBuilder :: Value -> Type -> Builder ()
valueBuilder (Boolean b) _ = emit $ if b then "true" else "false"
valueBuilder (Integer i) _ = emit (show i)
valueBuilder (Floating f) _ = emit (show f) -- TODO
valueBuilder (String s) _ = emit (show s)
valueBuilder (Array arr) t
    | Type.isByteArray t = do
        emit "\""
        forM_ (toList arr) $ \v -> hexadecimalBuilder v (Type.innerType t)
        emit "\""
    | otherwise          = case toList arr of
        []     -> emit "[]"
        [x]    -> emit "[ " >> indent 2 >> valueBuilder x (Type.innerType t) >> unindent >> emit " ]"
        (x:xs) -> do
            emit "[ " >> indent 2 >> valueBuilder x (Type.innerType t) >> unindent >> newline
            forM_ xs $ \v -> do
                emit ", "
                indent 2 >> valueBuilder v (Type.innerType t) >> unindent
                newline
            emit "]"
valueBuilder (Object obj) _ = case toList obj of
    [] -> emit "{}"
    [(k,v)] -> do
        let str = keyToString k <> " = "
        emit "{ " >> emit str >> indent (2 + length str) >> valueBuilder v (getValueType v) >> unindent >> emit " }"
    (k1,v1):xs -> do
        let str = "{ " <> keyToString k1 <> " = "
        emit str >> indent (length str) >> valueBuilder v1 (getValueType v1) >> unindent >> newline
        forM_ xs $ \(k, v) -> do
            let str' = ", " <> keyToString k <> " = "
            emit str' >> indent (length str) >> valueBuilder v (getValueType v) >> unindent
            newline
        emit "}"


hexadecimalBuilder :: Value -> Type -> Builder ()
hexadecimalBuilder (Integer x) Type.Signed8    = pad 2 x
hexadecimalBuilder (Integer x) Type.Unsigned8  = pad 2 x
hexadecimalBuilder (Integer x) Type.Signed16   = pad 4 x
hexadecimalBuilder (Integer x) Type.Unsigned16 = pad 4 x
hexadecimalBuilder (Integer x) Type.Signed32   = pad 8 x
hexadecimalBuilder (Integer x) Type.Unsigned32 = pad 8 x
hexadecimalBuilder (Integer x) Type.Signed64   = pad 16 x
hexadecimalBuilder (Integer x) Type.Unsigned64 = pad 16 x
hexadecimalBuilder v t = error $
    "tried to write into hexadecimal the value: " <> show v <> "\n" <>
    "With the type " <> show t <> ". But this is not a supported type \n" <>
    "For this operation."

pad :: Word -> Integer -> Builder ()
pad n = emit . go 0
  where
    go k v
      | v == 0 = replicate (toCount $ fromIntegral $ n - k) '0'
      | k > n = ""
      | otherwise =
        let (q, r) = divMod v 16
         in go (k + 1) r <> (showHex q)

    showHex 0  = "0"
    showHex 1  = "1"
    showHex 2  = "2"
    showHex 3  = "3"
    showHex 4  = "4"
    showHex 5  = "5"
    showHex 6  = "6"
    showHex 7  = "7"
    showHex 8  = "8"
    showHex 9  = "9"
    showHex 10 = "a"
    showHex 11 = "b"
    showHex 12 = "c"
    showHex 13 = "d"
    showHex 14 = "e"
    showHex 15 = "f"
    showHex _  = error "impossible happened"

-- Parser ---------------------------------------------------------------------

-- | parses a Value
valueParser :: Parser Value
valueParser =   parserBool
            <|> parserDouble
            <|> parserInteger
            <|> parserString
            <|> (Array <$> arrayParser)
            <|> (Object <$> objectParser)

parserBool :: Parser Value
parserBool = fmap Boolean $ (Parser.elements "true" >> pure True)
                        <|> (Parser.elements "false" >> pure False)
                        <|> Parser.reportError (Parser.Expected "true or false" "")

parserInteger :: Parser Value
parserInteger = fmap Integer $ do
    r <- Parser.takeWhile_ (`elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
    case readInteger r of
        Nothing -> Parser.reportError (Parser.Expected "Integer" ("got " <> r))
        Just v  -> pure v

parserDouble :: Parser Value
parserDouble = fmap Floating $ do
    r <- Parser.takeWhile_ (`elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
    Parser.element '.'
    d <- Parser.takeWhile_ (`elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
    mexp <- Parser.optional $ do
                Parser.element 'e'
                ("e" <>) <$> Parser.takeWhile_ (`elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-'])
    case readDouble (r <> "." <> d <> maybe mempty id mexp) of
        Nothing -> Parser.reportError (Parser.Expected "Double" ("received invalid: " <> r))
        Just v  -> pure v

parserString :: Parser Value
parserString = fmap String $ Parser.element '"' *> quotedParser <* Parser.element '"'
  where
    quotedParser = do
        s <- Parser.takeWhile_ ('"' /=)
        case unsnoc s of
            Just (_, '\\') -> Parser.skip 1 >> mappend (snoc s '"') <$> quotedParser
            _              -> pure s

-- | parses an array. Expect element of the array to be of the same type
arrayParser :: Parser Array
arrayParser = do
    Parser.element '['
    Parser.whiteSpacesAndNewLines
    l <- go <|> pure []
    Parser.whiteSpacesAndNewLines
    Parser.element ']'
    case mkArray l of
        Nothing -> Parser.reportError (Parser.Expected "Same typed values in array" "incompatible types")
        Just ar -> pure ar
  where
    go = do
        Parser.whiteSpacesAndNewLines
        r <- valueParser
        Parser.whiteSpacesAndNewLines
        (Parser.element ',' *> ((:) r <$> go)) <|> pure [r]

-- | parses an object
objectParser :: Parser Object
objectParser = fmap ObjectDef $ do
    Parser.element '{'
    Parser.whiteSpacesAndNewLines
    l <- go <|> pure []
    Parser.whiteSpacesAndNewLines
    Parser.element '}'
    pure l
  where
    go = do
        Parser.whiteSpacesAndNewLines
        n <- keyParser
        Parser.whiteSpacesAndNewLines
        Parser.element '='
        Parser.whiteSpacesAndNewLines
        r <- valueParser
        Parser.whiteSpacesAndNewLines
        (Parser.element ',' *> ((:) (n, r) <$> go)) <|> pure [(n, r)]

-- Gen ------------------------------------------------------------------------

-- type alias, the depth we are recuring when constructing a given Value
type Depth = Zn64 8

defaultDepth :: Depth
defaultDepth = zn64 4

genValue :: Depth -> Gen Value
genValue n
  | n == 0 = oneof $ nonEmpty_
    [ Boolean <$> arbitrary
    , Integer <$> arbitrary
    , Floating <$> arbitrary
    , String <$> arbitrary
    ]
  | otherwise = frequency $ nonEmpty_
    [ (15, Boolean <$> arbitrary)
    , (15, Integer <$> arbitrary)
    , (15, Floating <$> arbitrary)
    , (20, String <$> arbitrary)
    , (10, Array <$> genArray (n - 1))
    , ( 5, Object <$> genObject (n - 1))
    ]

genValueType :: Depth -> Gen (Gen Value)
genValueType n
  | n == 0 = elements $ nonEmpty_
    [ Boolean <$> arbitrary
    , Integer <$> arbitrary
    , Floating <$> arbitrary
    , String <$> arbitrary
    ]
  | otherwise = elements $ nonEmpty_
    [ Boolean <$> arbitrary
    , Integer <$> arbitrary
    , Floating <$> arbitrary
    , String <$> arbitrary
    , Array <$> genArray (n - 1)
    , Object <$> genObject (n - 1)
    ]

genArray :: Depth -> Gen Array
genArray n = do
    len <- fromIntegral <$> between (0, 31)
    gen <- genValueType n
    ArrayDef <$> replicateM len gen

genObject :: Depth -> Gen Object
genObject n = do
    len <- fromIntegral <$> between (0, 31)
    keys <- replicateM len arbitrary
    vals <- replicateM len (genValue n)
    pure $ ObjectDef $ F.zip keys vals
