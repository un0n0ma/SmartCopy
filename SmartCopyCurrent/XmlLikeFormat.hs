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
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

serializeSmart a = runSerialization (writeSmart xmlLikeSerializationFormat a)
    where runSerialization m = do snd $ runWriter m

parseSmart :: SmartCopy a => String -> Fail a
parseSmart = runParser (readSmart xmlLikeParseFormat)
    where runParser action value = evalState (runFailT action) value

xmlLikeSerializationFormat :: SerializationFormat (Writer String)
xmlLikeSerializationFormat
    = SerializationFormat
    { withCons =
          \cons m ->
          do let conName = T.unpack $ cname cons
             tell $ openTag conName
             m
             tell $ closeTag conName
    , withField =
          \field m ->
              case field of
                Index i ->
                    (tell $ openTag $ show i) >> m >> (tell $ closeTag $ show i)
                Labeled lab ->
                    (tell $ openTag $ T.unpack lab) >> m >> (tell $ closeTag $ T.unpack lab)
    , withRepetition =
          \wf list ->
              forM_ (zip list (repeat "value")) $
              \el ->
                  do tell $ openTag $ snd el
                     wf $ fst el
                     tell $ closeTag $ snd el
    , writePrimitive =
          \prim ->
            case prim of
              PrimInt i -> tell $ show i
              PrimBool b -> tell $ show b
              PrimString s -> tell s
              PrimDouble d -> tell $ show d
    }
                

xmlLikeParseFormat :: ParseFormat (FailT (State String))
xmlLikeParseFormat
    = ParseFormat
    { readCons =
          \cons ->
              do str <- get
                 let conNames = map (T.unpack . cname . fst) cons
                     parsers = map snd cons
                 case length cons of
                   0 -> fail "Parsing failure. No constructor to look up."
                   _ ->
                       do con <- readOpen
                          case lookup con (zip conNames parsers) of
                            Just parser ->
                                 do rest <- get
                                    res <- parser
                                    _ <- readCloseWith con
                                    return res
                            f -> fail $
                                 "Parsing failure. Didn't find \
                                 \constructor for tag " ++ show con ++
                                 ". Only found " ++ show conNames ++ "."
    , readField =
          \field ma ->
              do str <- get
                 let elName
                        = case field of
                            Index i -> show i
                            Labeled lab -> T.unpack lab
                 _ <- readOpenWith elName           
                 res <- ma
                 _ <- readCloseWith elName
                 return res
    , readRepetition =
          \ma ->
              do let acc = []
                 whileJust enterElemMaybe $
                           \_ -> do { res <- ma; _ <- readCloseWith "value"; return res }
    , readPrim =
          do str' <- get
             let str = filter (/=' ') str'
             case reads str of
               [(prim, rest)] ->
                   do put rest
                      return $ PrimDouble prim
               [] ->
                   case take 4 str of
                     "True" ->
                         do put $ drop 4 str
                            return $ PrimBool True
                     _ ->
                        case take 5 str of
                          "False" ->
                               do put $ drop 5 str
                                  return $ PrimBool False
                          _ ->
                            do return $ PrimString str --- FIX
    }

openTag s = "<" ++ s ++ ">"
closeTag  s = "</" ++ s ++ ">"
unwrap s 
    | startswith "</" s && endswith ">" s = init $ drop 2 s
    | startswith "<" s && endswith ">" s = init $ drop 1 s
    | otherwise = s

dropLast n xs = take (length xs - n) xs

readOpen :: FailT (State String) String
readOpen =
    do str <- get
       case isTagOpen str of
         True ->
             do let ('<':tag, '>':after)  = L.span (/='>') str
                put after
                return tag
         False ->
             fail $ "Didn't find an opening tag at " ++ str ++ "."
    where isTagOpen s = (startswith "<" s) && (not $ startswith "</" s)
                 
readClose :: FailT (State String) String
readClose =
    do str <- get
       case startswith "</" str of
         True ->
             do let (tag, '>':after) = L.span (/='>') str
                put after
                return tag
         False ->
             fail $ "Didn't find a closing tag at " ++ str ++ "."

readOpenWith :: String -> FailT (State String) String
readOpenWith s =
    do str <- get
       case startswith (openTag s) str of
         True ->
            do let (tag, '>':after) = L.span (/='>') str
               put after
               return ""
         False ->
            fail $ "Didn't find an opening tag for " ++ s ++
                   " at " ++ str ++ "."

readCloseWith :: String -> FailT (State String) String
readCloseWith s =
    do str <- get
       case startswith (closeTag s) str of
         True ->
            do let (tag, '>':after) = L.span (/='>') str
               put after
               return ""
         False ->
            fail $ "Didn't find a closing tag for " ++ s ++
                   " at " ++ str ++ "."

                 {-
                     do _ <- readOpenWith "value"
                        res <- ma
                        _ <- readCloseWith "value"
                        return ma
                        -}
--readElemMaybe :: FailT (State (Maybe String)) (Maybe String)
enterElemMaybe =
    do str <- get
       case startswith (openTag "value") str of
         True ->
             liftM Just (readOpenWith "value")
         False ->
             return Nothing
