{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module StringFormat where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import MonadTypesInstances
import SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.List as L
import qualified Data.Text as T
import Data.String.Utils

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer


sFormat = stringSerializationFormat
pFormat = stringParseFormat

--- Run functions, versioned and unversioned

serializeSmart a = runSerialization (smartPut sFormat a)
    where runSerialization m = snd $ runWriter m

parseSmart :: SmartCopy a => String -> Fail a
parseSmart = runParser (smartGet pFormat)
    where runParser action = evalState (runFailT action)

serializeUnvers a = runSerialization (writeSmart stringSerializationFormatUnvers a)
    where runSerialization m = snd $ runWriter m

parseUnvers :: SmartCopy a => String -> Fail a
parseUnvers = runParser (readSmart stringParseFormatUnvers)
    where runParser action = evalState (runFailT action)

--- Formats, unversioned and versioned

stringSerializationFormatUnvers
    = stringSerializationFormat
    { writeVersion = \_ -> return ()
    , withVersion = const id
    , withRepetition =
          \rep ->
              do tell "["
                 case length rep of
                   0 -> tell ""
                   n -> do mapM_ (\a ->
                                  do writeSmart stringSerializationFormatUnvers a
                                     tell ",") (init rep)
                           writeSmart stringSerializationFormatUnvers $ last rep
                 tell "]"
    }

stringParseFormatUnvers
    = stringParseFormat
    { readVersion = return $ Version 0
    , readRepetition =
          do str' <- get
             let str = filter (/=' ') str'
             case str of
               '[':xs ->
                   do let (list, rest) = L.span (/= ']') xs
                      case rest of
                        ']':xs -> do
                            put list
                            res <- mapWithDelim (readSmart stringParseFormatUnvers) list []
                            put xs
                            return res 
                        _ -> fail $
                             "No ']' found to terminate list at " ++ str
               f      ->
                   fail $ "No '[' found to initiate list at " ++ str ++ "."
    }
    where mapWithDelim mb list acc =
              do let (listelem, listrest) = L.span (/= ',') list
                 case T.unpack $ T.strip $ T.pack listrest of
                   ',':xs -> do put listelem
                                parseElem <- mb
                                mapWithDelim mb xs (acc ++ [parseElem])
                   _ -> do put listelem
                           parseElem <- mb
                           return $ acc ++ [parseElem]

stringSerializationFormat :: SerializationFormat (Writer String)
stringSerializationFormat
    = SerializationFormat
    { writeVersion =
          \ver ->
              tell $ (show $ unVersion ver) ++ ": "
    , withVersion =
          \ver m ->
              writeVersion sFormat ver >> m
    , withCons =
          \cons ma ->
              do { tell $ T.unpack $ cname cons; ma }
    , withField =
          wrapM
    , withRepetition =
          \rep ->
              do let version = case length rep of
                                 0 -> Version 0
                                 _ -> versionFromProxy (mkProxy $ head rep)
                 writeVersion sFormat version >>
                     withRepetition stringSerializationFormatUnvers rep
    , writeInt =
          \prim ->
              case prim of
                PrimInt i    -> tell $ show i
                _            -> mismatch "Prim Int" (show prim)
    , writeChar =
          \prim ->
              case prim of
                PrimChar c   -> tell "c"
                _            -> mismatch "Prim Char" (show prim)
    , writeDouble =
          \prim ->
              case prim of
                PrimDouble d -> tell $ show d
                _            -> mismatch "Prim Double" (show prim)
    , writeString =
          \prim ->
              case prim of
                PrimString s -> tell s
                _            -> mismatch "Prim String" (show prim)
    , writeBool =
          \prim ->
              case prim of
                PrimBool b   -> tell $ show b
                _            -> mismatch "Prim Bool" (show prim)
    }
    where wrapM m = do { tell " ("; m; tell ") " }

                

stringParseFormat :: ParseFormat (FailT (State String))
stringParseFormat
    = ParseFormat
    { readVersioned = id
    , readVersion =
         do str' <- get
            let str = filter (/=' ') str'
                (version, after) = L.span (/=':') str
            case after of
              ':':xs ->
                  case reads version of
                    [(int, [])] ->
                        do put xs
                           return $ Version int
                    _ -> mismatch "Int32" version
              _ -> do put after
                      return $ Version 0
    , readCons =
         \cons ->
             do str <- get
                let conNames = map (cname . fst) cons
                    parsers = map snd cons
                case length cons of
                  0 -> noCons
                  _ ->
                     do con <- startCons
                        case lookup (T.pack con) (zip conNames parsers) of
                          Just parser ->
                             parser
                          f ->
                             fail $ "Parsing failure. Didnt't find constructor for tag "
                                   ++ show con ++ ". Only got " ++ show conNames
    , readField =
          \ma ->
              do rest <- readOpen
                 res <- ma
                 _ <- readClose
                 return res

    , readRepetition =
          readVersion pFormat >> readRepetition stringParseFormatUnvers
    , readInt =
          do str <- get
             let prim = filter (/=' ') str
             case reads prim of
               [(num, xs)] ->
                   do put xs
                      return $ PrimInt num
               [] -> mismatch "Int" prim
    , readChar =
         do str <- get
            let prim = filter (/=' ') str
            case length prim of
              0 -> mismatch "Char" prim
              _ ->
                  do put $ tail prim
                     return $ PrimChar $ head prim 
    , readBool =
          do str <- get
             let prim = filter (/=' ') str
             readBool' prim
    , readDouble =
          do str <- get
             let prim = filter (/=' ') str
             case reads prim of
               [(num, xs)] ->
                   do put xs
                      return $ PrimDouble num
               [] -> mismatch "Double" prim
    , readString =
          do str <- get
             let prim = filter (/=' ') str
             put $ snd $ delimit prim
             return $ PrimString $ fst $ delimit prim
    }
    where delimit                  = L.span (/=')')
          readBool' :: String -> FailT (State String) Prim
          readBool' prim
              | startswith "True" prim =
                do put $ drop 4 prim; return $ PrimBool True
              | startswith "False" prim =
                do put $ drop 5 prim; return $ PrimBool False
              | otherwise =
                mismatch "Bool" prim

startCons :: FailT (State String) String
startCons =
    do str' <- get
       let str = filter (/=' ') str'
       if hasNested str
          then do let (cons, after) = L.span (/='(') str
                  put after
                  return cons
          else do let (cons, after) = L.span (/=')') str
                  put after
                  return cons

readOpen :: FailT (State String) String
readOpen =
    do str' <- get
       let str = filter (/=' ') str'
       case str of
         '(':rest ->
             do put rest
                return ""
         _ ->
            fail $ "No opening parenthesis found at " ++ str ++ "."


hasNested str = let (untilClosedPar, _) = L.span (/=')') str
                    (untilOpenPar, _) = L.span (/='(') str
                in length untilClosedPar >= length untilOpenPar

readClose :: FailT (State String) String
readClose =
    do str <- get
       case filter (/=' ') str of
         ')':xs ->
             do put xs
                return ""
         _ -> fail $ "No closing parenthesis found at " ++ str ++ "."

