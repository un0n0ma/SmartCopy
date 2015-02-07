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


serializeSmart a = runSerialization (writeSmart stringSerializationFormat a)
    where runSerialization m = do snd $ runWriter m

parseSmart :: SmartCopy a => String -> Fail a
parseSmart = runParser (readSmart stringParseFormat)
    where runParser action value = evalState (runFailT action) value

stringSerializationFormat :: SerializationFormat (Writer String)
stringSerializationFormat
    = SerializationFormat
    { withCons =
          \cons ma ->
              do { tell $ T.unpack $ cname cons; ma }
    , withField =
          \ma -> wrapM ma
    , withRepetition =
          \rep ->
              do tell "["
                 case length rep of
                   0 -> tell ""
                   n -> do sequence_ $
                             map (\a ->
                                     do writeSmart (stringSerializationFormat) a
                                        tell ",") (init rep)
                           writeSmart (stringSerializationFormat) $ last rep
                 tell "]"

    , writePrimitive =
          \prim ->
              case prim of
                PrimInt i    -> tell $ show i
                PrimDouble d -> tell $ show d
                PrimString s -> tell s
                PrimBool b   -> tell $ show b
                f            -> fail $ "Was expecting primitive value but got " ++ show f
    }
    where wrapM m = do { tell " ("; m; tell ") " }

                

stringParseFormat :: ParseFormat (FailT (State String))
stringParseFormat
    = ParseFormat
    { readCons =
         \cons ->
             do str <- get
                let conNames = map (cname . fst) cons
                    parsers = map snd cons
                case length cons of
                  0 -> fail "Parsing failure. No constructor to look up."
                  _ ->
                     do con <- startCons
                        case lookup (T.pack con) (zip conNames parsers) of
                          Just parser ->
                             do parser
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
          do str' <- get
             let str = filter (/=' ') str'
             case str of
               '[':xs ->
                   do let (list, rest) = L.span (/= ']') xs
                      case rest of
                        ']':xs -> do
                            put list
                            res <- mapWithDelim (readSmart stringParseFormat) list []
                            put xs
                            return res 
                        _ -> fail $
                             "No ']' found to terminate list at " ++ str
               f      ->
                   fail $ "No '[' found to initiate list at " ++ str
    , readPrim =
          do str <- get
             let prim = filter (/=' ') str
             case reads prim of
               [(num, xs)] ->
                   do put xs
                      return $ PrimDouble num
               [] -> readBoolOrString prim
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
          readBoolOrString :: String -> FailT (State String) Prim
          readBoolOrString prim =
              if startswith "True" prim
                 then do { put $ drop 4 prim; return $ PrimBool True }
                 else if startswith "False" prim
                 then do { put $ drop 5 prim; return $ PrimBool False }
                 else do { put $ snd $ delimit prim; return $ PrimString $ (fst $ delimit prim) }
              where delimit str = L.span (/=')') str

startCons :: FailT (State String) String
startCons =
    do str' <- get
       let str = filter (/=' ') str'
       case hasNested str of
         True ->
             do let (cons, after) = L.span (/='(') str
                put after
                return cons
         False ->
             do let (cons, after) = L.span (/=')') str
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

