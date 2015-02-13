{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module XmlLikeFormat where

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
import Control.Applicative
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer


--- Run functions, versioned and unversioned

serializeSmart a = runSerialization (smartPut sFormat a)
    where runSerialization m = execWriter (runStateT m [])

parseSmart :: SmartCopy a => String -> Fail a
parseSmart = runParser (smartGet pFormat)
    where runParser action value = evalState (evalStateT (runFailT action) value) []

serializeUnvers a = runSerialization (writeSmart sFormatUnvers a)
    where runSerialization m = execWriter (runStateT m [])

parseUnvers :: SmartCopy a => String -> Fail a
parseUnvers = runParser (readSmart pFormatUnvers)
    where runParser action value = evalState (evalStateT (runFailT action) value) []

--- Xml-formats, unversioned and versioned

sFormatUnvers
    = sFormat
    { withVersion = const id
    , writeVersion = \_ -> return ()
    , writeRepetition =
          \list ->
              forM_ (zip list (repeat "value")) $
              \el ->
                  do tell $ openTag $ snd el
                     writeSmart sFormat $ fst el
                     tell $ closeTag $ snd el
    , writeMaybe =
          \m ->
            case m of
              Just a -> writeSmart sFormatUnvers a
              Nothing -> return ()
    }

pFormatUnvers
    = pFormat
    { readVersioned = id
    , readVersion = return $ Version 1
    , readMaybe =
          liftM Just (readSmart pFormatUnvers) <|>
          do _ <- readClose; return Nothing
    }
    where delimit = L.span (/='<')
    
sFormat :: SerializationFormat (StateT [String] (Writer String))
sFormat
    = SerializationFormat
    { withVersion =
          \ver m -> writeVersion sFormat ver >> m
    , writeVersion =
          \ver -> tell $ "<?version=" ++ show (unVersion ver) ++ "?>"
    , withCons =
          \cons m ->
          do let conName = T.unpack $ cname cons
             tell $ openTag conName
             let fields
                    = case cfields cons of
                        NF 0 ->
                            []
                        NF i ->
                            map show [0..i-1]
                        LF ls ->
                            map T.unpack ls
             put fields
             m
             tell $ closeTag conName
    , withField =
          \m ->
              do fields <- get
                 case fields of
                   field:rest ->
                       do tell $ openTag field
                          m
                          put rest
                          tell $ closeTag field
                   [field] ->
                       do tell $ openTag field
                          m
                          tell $ closeTag field
                   [] -> m
    , writeRepetition =
          \list ->
              case length list of
                0 -> return ()
                n -> withVersion sFormat version $
                     forM_ (zip list (repeat "value")) $
                     \el ->
                         do tell $ openTag $ snd el
                            writeSmart sFormat $ fst el
                            tell $ closeTag $ snd el
                     where version = versionFromProxy (mkProxy $ head list)
    , writeInt =
          \prim ->
            case prim of
              PrimInt i -> tell $ show i
              _         -> mismatch "Prim Int" (show prim)
    , writeChar =
          \prim ->
            case prim of
              PrimChar c -> tell "c"
              _         -> mismatch "Prim Char" (show prim)
    , writeString =
          \prim ->
            case prim of
              PrimString s -> tell s
              _         -> mismatch "Prim String" (show prim)
    , writeBool =
          \prim ->
            case prim of
              PrimBool b -> tell $ show b
              _         -> mismatch "Prim Bool" (show prim)
    , writeDouble =
          \prim ->
            case prim of
              PrimDouble d -> tell $ show d
              _         -> mismatch "Prim Double" (show prim)
    , writeMaybe =
          \m ->
            case m of
              Just a -> smartPut sFormat a
              Nothing -> return ()
    }
                

pFormat :: ParseFormat (FailT (StateT String (State [String])))
pFormat
    = ParseFormat
    { readVersioned = id
    , readVersion =
        do str' <- get
           let str = filter (/=' ') str'
           if startswith "<?version=" str
              then do let (version, rest) = L.span (/='?') (drop 10 str)
                      if startswith "?>" rest
                         then 
                           case reads version of
                             [(int, [])] ->
                                 do lift $ put $ drop 2 rest
                                    return $ Version int
                             [] -> mismatch "Int32" version
                         else mismatch "closing tag for version" str
              else do lift $ put str
                      return $ Version 0
    , readCons =
          \cons ->
              do str <- get
                 let conNames = map (T.unpack . cname . fst) cons
                     conFields = map (cfields . fst) cons
                     parsers = map snd cons
                 case length cons of
                   0 -> noCons
                   _ ->
                       do con <- readOpen
                          case lookup con (zip conNames parsers) of
                            Just parser ->
                                 do let Just cfields = lookup con (zip conNames conFields)
                                        fields
                                            = case cfields of
                                                NF i -> map show [0..i-1]
                                                LF lbs -> map T.unpack lbs
                                    lift $ lift $ put fields
                                    rest <- get
                                    res <- parser
                                    _ <- readCloseWith con
                                    return res
                            _ -> conLookupErr (show con) (show conNames)

    , readField =
          \ma ->
              do str <- get
                 fields <- lift $ lift get
                 case fields of
                   [] -> ma
                   (x:xs) ->
                       do _ <- readOpenWith x
                          res <- ma
                          _ <- readCloseWith x
                          lift $ lift $ put xs
                          return res
    , readRepetition =
          do readVersion pFormat
             whileJust enterElemMaybe $
                 \_ ->
                     do res <- readSmart pFormat
                        _ <- readCloseWith "value"
                        return res
    , readInt =
          do str' <- get
             let str = filter (/=' ') str'
             case reads str of
               [(prim, rest)] ->
                   do lift $ put rest
                      return $ PrimInt prim
               [] -> mismatch "Int" str
    , readDouble =
          do str' <- get
             let str = filter (/=' ') str'
             case reads str of
               [(prim, rest)] ->
                   do lift $ put rest
                      return $ PrimDouble prim
               [] -> mismatch "Double" str
    , readBool =
          do str' <- get
             let str = filter (/=' ') str'
             case take 4 str of
               "True" ->
                   do lift $ put $ drop 4 str
                      return $ PrimBool True
               _ ->
                  case take 5 str of
                    "False" ->
                         do lift $ put $ drop 5 str
                            return $ PrimBool False
                    _ -> mismatch "Bool" str
    , readString =
          do str' <- get
             let str = filter (/=' ') str'
             do lift $ put $ snd $ delimit str
                return $ PrimString $ fst $ delimit str
    , readChar =
          do str' <- get
             let str = filter (/=' ') str'
             case length str of
               0 -> mismatch "Char" str
               _ ->    
                   do lift $ put $ tail str
                      return $ PrimChar $ head str
    , readMaybe =
          liftM Just (smartGet pFormat) <|>
          do _ <- readClose; return Nothing
    }
    where delimit = L.span (/='<')

openTag s = "<" ++ s ++ ">"
closeTag  s = "</" ++ s ++ ">"
unwrap s 
    | startswith "</" s && endswith ">" s = init $ drop 2 s
    | startswith "<" s && endswith ">" s = init $ drop 1 s
    | otherwise = s

dropLast n xs = take (length xs - n) xs

readOpen :: FailT (StateT String (State [String])) String
readOpen =
    do str <- get
       if isTagOpen str
          then do let ('<':tag, '>':after)  = L.span (/='>') str
                  put after
                  return tag
          else fail $ "Didn't find an opening tag at " ++ str ++ "."
    where isTagOpen s = startswith "<" s && (not $ startswith "</" s)
                 
readClose :: FailT (StateT String (State [String])) String
readClose =
    do str <- get
       if startswith "</" str
          then do let (tag, '>':after) = L.span (/='>') str
                  put after
                  return tag
          else fail $ "Didn't find a closing tag at " ++ str ++ "."

readOpenWith :: String -> FailT (StateT String (State [String])) String
readOpenWith s =
    do str <- get
       if startswith (openTag s) str
          then do let (tag, '>':after) = L.span (/='>') str
                  put after
                  return ""
          else fail $ "Didn't find an opening tag for " ++ s ++
                      " at " ++ str ++ "."

readCloseWith :: String -> FailT (StateT String (State [String])) String
readCloseWith s =
    do str <- get
       if startswith (closeTag s) str
          then do let (tag, '>':after) = L.span (/='>') str
                  put after
                  return ""
          else fail $ "Didn't find a closing tag for " ++ s ++
                      " at " ++ str ++ "."

enterElemMaybe =
    do str <- get
       if startswith (openTag "value") str
          then liftM Just (readOpenWith "value")
          else return Nothing
