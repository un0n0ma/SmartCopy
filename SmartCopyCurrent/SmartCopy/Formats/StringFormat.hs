{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}


module SmartCopy.Formats.StringFormat
       ( serializeUnvers
       , parseUnvers
       , serializeSmart
       , parseSmart
       )
where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import SmartCopy.Instances
import SmartCopy.MonadTypesInstances
import SmartCopy.SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as L (span)
import qualified Data.Text as T
import Data.List.Utils (startswith)

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer

-------------------------------------------------------------------------------
-- Run functions, versioned and unversioned
-------------------------------------------------------------------------------

serializeSmart a = runSerialization (smartPut sFormat a)
    where runSerialization m = snd $ runWriter m

parseSmart :: SmartCopy a => String -> Fail a
parseSmart = runParser (smartGet pFormat)
    where runParser action = evalState (runFailT action)

serializeUnvers a = runSerialization (writeSmart sFormatUnvers a)
    where runSerialization m = snd $ runWriter m

parseUnvers :: SmartCopy a => String -> Fail a
parseUnvers = runParser (readSmart pFormatUnvers)
    where runParser action = evalState (runFailT action)

-------------------------------------------------------------------------------
-- Unversioned serialization
-------------------------------------------------------------------------------

sFormatUnvers
    = sFormat
    { mkPutter = \_ -> return $ writeSmart sFormatUnvers 
    , writeRepetition =
          \rep ->
              do tell "["
                 case length rep of
                   0 -> tell ""
                   n -> do mapM_ (\a ->
                                  do writeSmart sFormatUnvers a
                                     tell ",") (init rep)
                           writeSmart sFormatUnvers $ last rep
                 tell "]"
    , writeMaybe =
          \ma ->
              case ma of
                Nothing -> tell ""
                Just a -> writeSmart sFormatUnvers a
    }

-------------------------------------------------------------------------------
-- Unversioned parsing
-------------------------------------------------------------------------------

pFormatUnvers
    = pFormat
    { readRepetition =
          do str' <- get
             let str = filter (/=' ') str'
             case str of
               '[':xs ->
                   do let (list, rest) = L.span (/= ']') xs
                      case rest of
                        ']':xs -> do
                            put list
                            res <- mapWithDelim (readSmart pFormatUnvers) list []
                            put xs
                            return res 
                        _ -> mismatch "]" rest
               _      -> mismatch "[" str'
    , readMaybe =
          do str' <- get
             let str = filter (/=' ') str'
             case str of
               ')':xs ->
                   do put str
                      return Nothing
               _ -> liftM Just $ readSmart pFormatUnvers
    }

-------------------------------------------------------------------------------
-- Versioned serialization
-------------------------------------------------------------------------------

sFormat :: SerializationFormat (Writer String)
sFormat
    = SerializationFormat
    { mkPutter =
          \ver ->
              do wrapM $ tell $ "version:" ++ show (unVersion ver)
                 return $ writeSmart sFormat
    , withCons =
          \cons ma ->
              do { tell $ T.unpack $ cname cons; ma }
    , withField =
          wrapM
    , writeRepetition =
          \rep ->
              do tell "["
                 case rep of
                   [] -> tell ""
                   (x:xs) ->
                       do putter <- getSmartPut sFormat
                          putter x
                          tell ","
                          mapM_ (\a ->
                                    do writeSmart sFormat a
                                       tell ",") $ init xs
                          writeSmart sFormat $ last xs
                 tell "]"
    , writeInt = tell . show
    , writeInteger = tell . show
    , writeChar = tell . show
    , writeDouble = tell . show
    , writeString = tell
    , writeBool = tell . show
    , writeMaybe =
          \ma ->
              case ma of
                Nothing -> tell ""
                Just a -> smartPut sFormat a
    , writeBS = tell . BSC.unpack
    , writeText = tell . T.unpack
    }
    where wrapM m = do { tell " ("; m; tell ")" }

                
-------------------------------------------------------------------------------
-- Versioned parsing
-------------------------------------------------------------------------------

pFormat :: ParseFormat (FailT (State String))
pFormat
    = ParseFormat
    { mkGetter =
        do let kind = kindFromProxy (Proxy :: Proxy a)
           version <- readVersion
           case version of
             Just v ->
                 case constructGetterFromVersion pFormat v kind of
                         Right getter -> return getter
                         Left msg -> fail msg
             Nothing -> return $ readSmart pFormat
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
                          f -> conLookupErr (show con) (show conNames)
    , readField =
          \ma ->
              do rest <- readOpen
                 res <- ma
                 _ <- readClose
                 return res

    , readRepetition =
          do readVersion
             str' <- get
             let str = filter (/=' ') str'
             case str of
               '[':xs ->
                   do let (list, rest) = L.span (/=']') xs
                      case rest of
                        ']':xs' ->
                            do put list
                               getter <- getSmartGet pFormat
                               res <- mapWithDelim getter list []
                               put xs'
                               return res
                        _ -> mismatch "]" rest
               _ -> mismatch "[" str'
    , readInt =
          do str <- get
             let prim = filter (/=' ') str
             case reads prim of
               [(num, xs)] ->
                   do put xs
                      return num
               [] -> mismatch "Int" prim
    , readChar =
         do str <- get
            let prim = filter (/=' ') str
            case length prim of
              0 -> mismatch "Char" prim
              _ ->
                  do put $ tail prim
                     return $ head prim 
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
                      return num
               [] -> mismatch "Double" prim
    , readString =
          do str <- get
             let prim = filter (/=' ') str
             put $ snd $ delimit prim
             return $ fst $ delimit prim
    , readMaybe =
          do str' <- get
             let str = filter (/=' ') str'
             case str of
               ')':xs ->
                   do put str
                      return Nothing
               _ -> liftM Just $ smartGet pFormat
    , readBS =
         do str <- get
            let prim = filter (/=' ') str
            put $ snd $ delimit prim
            return $ BSC.pack $ fst $ delimit prim
    , readText =
         do str <- get
            let prim = filter (/=' ') str
            put $ snd $ delimit prim
            return $ T.pack $ fst $ delimit prim
    }
    where delimit                  = L.span (/=')')
          readBool' :: String -> FailT (State String) Bool
          readBool' prim
              | startswith "True" prim =
                do put $ drop 4 prim; return True
              | startswith "False" prim =
                do put $ drop 5 prim; return False
              | otherwise =
                mismatch "Bool" prim
          readVersion =
              do str' <- get
                 let str = filter (/=' ') str'
                     (untilVer, after) = L.span (/=':') str
                 case after of
                   ':':xs ->
                        case reads xs of
                          [(int, ')':afterVer)] ->
                              do let withoutVer = take (length untilVer - 8) untilVer ++ afterVer
                                 put withoutVer
                                 return $ Just $ Version int
                          _ -> mismatch "int32" after
                   _ -> return Nothing


-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

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

mapWithDelim mb list acc
    | null list
    = return []
    | otherwise
    = do let (listelem, listrest) = L.span (/= ',') list
         case T.unpack $ T.strip $ T.pack listrest of
           ',':xs -> do put listelem
                        parseElem <- mb
                        mapWithDelim mb xs (acc ++ [parseElem])
           _ -> do put listelem
                   parseElem <- mb
                   return $ acc ++ [parseElem]
