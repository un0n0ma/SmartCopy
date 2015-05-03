{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |Formats for serialization and deserialization of a datatype in a simple
-- XML representation. Unversioned or additionally supporting version control
-- and writing out version tags as XML attributes.
module Data.SmartCopy.Formats.XmlLike
       ( serializeUnvers
       , parseUnvers
       , serializeSmart
       , parseSmart
       , serializeLastKnown
       , parseLastKnown
       , toXmlString
       , fromXmlString
       )
where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import Data.SmartCopy
import Data.SmartCopy.MonadTypesInstances (FailT, runFailT)
import Data.SmartCopy.SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import Data.String.Utils

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC (pack, unpack)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Text.XML as X
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC (pack, unpack)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Text.XML as X

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Control.Applicative
import Control.Arrow
import Control.Monad.Loops
import Data.Either (lefts, rights)

import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer

-------------------------------------------------------------------------------
-- Run functions, versioned and unversioned
-------------------------------------------------------------------------------

-- |Convert a datatype made an instance of SmartCopy into a versioned XML 
-- representation.
serializeSmart :: SmartCopy a => a -> X.Element
serializeSmart a
    = runSerialization (smartPut sFormat a)

-- |Parse a datatype made an instance of SmartCopy from a versioned XML
-- representation.
parseSmart :: SmartCopy a => X.Element -> Fail a
parseSmart
    = runParser (smartGet pFormat)

-- |Convert a datatype made an instance of SmartCopy into an unversioned XML
-- representation.
serializeUnvers :: SmartCopy a => a -> X.Element
serializeUnvers a
    = runSerialization (writeSmart sFormatUnvers a Nothing)
    where runSerialization m = execState (evalStateT m []) emptyEl

-- |Parse a datatype made an instance of SmartCopy from an unversioned XML
-- representation.
parseUnvers :: SmartCopy a => X.Element -> Fail a
parseUnvers
    = runParser (readSmart pFormatUnvers)
    where runParser action el =
              evalState (evalStateT (evalStateT (runFailT action) [X.NodeElement el]) el) []

-- |Check if a datatype version is known by a communicating component,
-- indicated by its identifier being present in the list of all known
-- identifiers. Convert the latest known version of the datatype into its
-- versioned XML representation.
serializeLastKnown :: SmartCopy a => a -> [String] -> X.Element
serializeLastKnown a ids
    = runSerialization (smartPutLastKnown sFormatBackComp a ids)

-- |Parse a versioned datatype serialized using the back-compatible XML format.
parseLastKnown :: SmartCopy a => X.Element -> Fail a
parseLastKnown
    = runParser (smartGet pFormatBackComp)
    
-- |Convert an XML root element to a string, using Data.XML functions.
toXmlString = TL.unpack . (\doc -> X.renderText X.def doc) . (\m -> X.Document (X.Prologue [] Nothing []) m [])

runSerialization m = execState (evalStateT m []) emptyEl

runParser action el =
       evalState (evalStateT (evalStateT (runFailT action) [X.NodeElement el]) el) []

-- |Obtain an XML root element from an input string, using Data.XML functions.
fromXmlString value =
    let (X.Document _ el _) = X.parseText_ X.def (TL.pack value)
    in el

-------------------------------------------------------------------------------
-- Xml serialization unversioned
-------------------------------------------------------------------------------

sFormatUnvers
    = sFormat
    { mkPutter = \_ v _ -> return $ \a -> writeSmart sFormatUnvers a Nothing
    , writeRepetition =
          \ar _ ->
              do let arrAttr = M.fromList [("type", "array")]
                 let value = X.Element (makeName (T.pack "value")) M.empty []
                 case length ar of
                   0 -> do put [X.NodeElement value]
                           lift $ put $ X.Element (makeName "PrimList") arrAttr []
                           return ()
                   n -> do let value = X.Element (makeName (T.pack "value")) M.empty []
                           put $ replicate n (X.NodeElement value)
                           forM_ ar $ \a ->
                               do withField sFormatUnvers $ writeSmart sFormatUnvers a Nothing
                                  field <- get
                                  put field
                           res <- get
                           put res
                           lift $ put $ X.Element (makeName "PrimList") arrAttr res
    , writeMaybe =
          \ma _ ->
              case ma of
                Just a ->
                    do let optAttr = M.fromList [("type", "opt")]
                       writeSmart sFormatUnvers a Nothing
                       el <- lift get
                       lift $ put $ X.Element (X.elementName el) optAttr (X.elementNodes el)
                Nothing ->
                    do put []
                       return ()
    }

-------------------------------------------------------------------------------
-- Xml parsing unversioned
-------------------------------------------------------------------------------

pFormatUnvers
    = pFormat
    { mkGetter = \_ _ _ -> return $ readSmart pFormatUnvers
    , readRepetition =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case length nodes of
               0 ->
                   case el' of
                     X.Element (X.Name "PrimList" _ _) _ nodes' ->
                         do lift $ lift $ lift $ put nodes'
                            replicateM (length nodes') (readField pFormatUnvers $
                                readSmart pFormatUnvers)
                     _ -> return []
               n ->
                   replicateM n (readField pFormatUnvers $ readSmart pFormatUnvers)
    , readMaybe =
          do nodes <- lift $ lift $ lift get
             case nodes of
               [] -> return Nothing
               X.NodeElement el:xs ->
                   case X.elementNodes el of
                     [] -> return Nothing
                     elNodes ->
                         do put elNodes
                            lift $ lift $ lift $ put xs
                            liftM Just $ readSmart pFormatUnvers
               cont@[X.NodeContent val] ->
                   do put cont
                      liftM Just $ readSmart pFormatUnvers
               h:_ -> mismatch "Maybe node" (show h)
    }

    
-------------------------------------------------------------------------------
-- Xml serialization versioned with back-migration
-------------------------------------------------------------------------------
sFormatBackComp
    = sFormat
    { mkPutter =
          \b ver mIds ->
              case mIds of
                Just _ ->
                    return $ \a ->
                        do writeSmart sFormatBackComp a mIds
                           resEl <- lift get
                           let versEl = resEl
                                      { X.elementAttributes =
                                      M.insert (makeName (T.pack "version"))
                                      (T.pack $ show ver)
                                      (X.elementAttributes resEl) }
                           lift $ put versEl
                Nothing ->
                    fail $ noIDListErr "[type not yet known]"
    , withCons =
          \cons ma ->
              do let fields =
                          case cfields cons of
                            Empty -> []
                            NF i -> map (T.pack . (++) "Field" . show) [0..i-1]
                            LF ls -> ls
                 let nodes = map makeEmptyNode fields
                 put nodes
                 ma
                 resNodes <- get
                 put [X.NodeElement $ X.Element
                      (makeName $ T.pack $ cidentifier cons ++ "Ind" ++ show (cindex cons))
                      M.empty resNodes]
                 lift $ put $ X.Element
                     (makeName $ T.pack $ cidentifier cons ++ "Ind" ++ show (cindex cons))
                     M.empty resNodes
    , writeRepetition =
          \ar mIds ->
              case mIds of
                Just allIds ->
                    do let value = X.Element (makeName (T.pack "value")) M.empty []
                           arrAttr = M.fromList [("type", "array")]
                       case ar of
                         [] ->
                             do put [X.NodeElement value]
                                lift $ put $ X.Element (makeName "PrimList") arrAttr []
                                return ()
                         h:t ->
                             do lift $ put value
                                putter <- getSmartPutLastKnown sFormatBackComp allIds
                                unversHead <- lift get
                                put [X.NodeElement unversHead]
                                withField sFormatBackComp $ putter h
                                versHead <- get
                                put $ replicate (length ar - 1) (X.NodeElement value)
                                forM_ t $ \a ->
                                     do withField sFormatBackComp $ writeSmart sFormatBackComp a Nothing
                                        field <- get
                                        put field
                                res <- get
                                put $ versHead++res
                                lift $ put $
                                    X.Element (makeName "PrimList") arrAttr (versHead++res)
                Nothing ->
                    fail $ noIDListErr "SmartCopy a => [a]"
    , writeMaybe =
          \ma mIds ->
              case mIds of
                Just allIds ->
                    case ma of
                      Just a ->
                          do let optAttr = M.fromList [("type", "opt")]
                             putter <- getSmartPutLastKnown sFormatBackComp allIds
                             putter a
                             el <- lift get
                             lift $ put $ X.Element (X.elementName el) optAttr (X.elementNodes el)
                      Nothing ->
                          do put []
                             return ()
                Nothing ->
                    fail $ noIDListErr "SmartCopy a => Maybe a"
    }
    

-------------------------------------------------------------------------------
-- Versioned parsing with back-migration
-------------------------------------------------------------------------------

pFormatBackComp
    = pFormat
    { mkGetter =
          \_ _ prevVers ->
              do nodeElems <- get
                 case nodeElems of
                   [] ->
                       return $ readSmart pFormatBackComp
                   _ ->
                       do el <- pop
                          case el of
                            X.NodeContent _ ->
                                return $ readSmart pFormatBackComp
                            X.NodeElement el ->
                                do vers <- readVersion el
                                   case prevVers of
                                     Just p ->
                                         case constructGetterFromVersion pFormatBackComp (Version p) kind of
                                           Right getter ->
                                               return getter
                                           Left msg ->
                                               fail msg
                                     Nothing ->
                                         case vers of
                                           Just v ->
                                               case constructGetterFromVersion pFormatBackComp v kind of
                                                 Right getter ->
                                                     return getter
                                                 Left msg ->
                                                     fail msg
                                           Nothing -> 
                                               return $ readSmart pFormatBackComp
                            _ ->
                                return $ mismatch "NodeContent or NodeElement" (show nodeElems)
    , readCons =
          \cons ->
              do elem <- lift $ lift get
                 case cons of
                   [] -> noCons
                   x:_ -> 
                       do let con = elName elem
                              conId = cidentifier (fst x) ++ "Ind"
                              conLkp = map (T.pack . (++) conId . show . cindex . fst) cons
                              parsers = map snd cons
                          case lookup con (zip conLkp parsers) of
                            Just parser ->
                                do lift $ lift $ lift $ put $ X.elementNodes elem
                                   put $ X.elementNodes elem
                                   parser
                            _ -> conLookupErr (show con) (show conLkp)
    , readRepetition =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case nodes of
               [] ->
                   case el' of
                     X.Element (X.Name "PrimList" _ _) _ nodes' ->
                         do lift $ lift $ lift $ put nodes'
                            readListVals nodes'
                     _ -> return []
               _ -> readListVals nodes
    , readMaybe =
          do nodes <- lift $ lift $ lift get
             case nodes of
               [] -> return Nothing
               X.NodeElement el:xs ->
                   case X.elementNodes el of
                     [] -> return Nothing
                     elNodes ->
                         do put nodes
                            lift $ lift $ lift $ put xs
                            getSmartGet pFormatBackComp >>= liftM Just
               cont@[X.NodeContent x] ->
                   do put cont
                      getSmartGet pFormatBackComp >>= liftM Just 
               h:_ -> mismatch "Maybe node" (show h)
   }
   where readListVals :: SmartCopy a
                       => [X.Node]
                       -> FailT (StateT [X.Node] (StateT X.Element (State [X.Node]))) [a]
         readListVals nodes =
             do put nodes
                getter <- getSmartGet pFormatBackComp
                replicateM (length nodes) $ readField pFormatBackComp getter

-------------------------------------------------------------------------------
-- Xml serialization versioned
-------------------------------------------------------------------------------

sFormat :: SerializationFormat (StateT [X.Node] (State X.Element))
sFormat
    = SerializationFormat
    { mkPutter =
          \_ ver _ ->
              return $ \a ->
                  do writeSmart sFormat a Nothing
                     resEl <- lift get
                     let versEl = resEl
                                { X.elementAttributes =
                                      M.insert (makeName (T.pack "version"))
                                               (T.pack $ show ver)
                                               (X.elementAttributes resEl) }
                     lift $ put versEl
    , withCons =
          \cons ma ->
              do let fields =
                          case cfields cons of
                            Empty -> []
                            NF i -> map (T.pack . (++) "Field" . show) [0..i-1]
                            LF ls -> ls
                 let nodes = map makeEmptyNode fields
                 put nodes
                 ma
                 resNodes <- get
                 put [X.NodeElement $ X.Element (makeName $ cname cons) M.empty resNodes]
                 lift $ put $ X.Element (makeName $ cname cons) M.empty resNodes
    , withField =
          \ma ->
              do nodes <- get
                 case nodes of
                   [] -> return ()
                   x@(X.NodeElement elem):xs ->
                       do lift $ put elem
                          ma
                          resVers <- lift get
                          resNodes <- get
                          let resElems = map (X.Element (X.elementName elem) (X.elementAttributes resVers)) [resNodes]
                          let nodeEls = map X.NodeElement resElems
                          put $ xs++nodeEls
                          lift $ put $ 
                              X.Element (X.elementName resVers)
                                        (X.elementAttributes resVers) resNodes
                   _ -> mismatch "NodeElement" (show nodes)
    , writeInt =
          \i ->
              do let resNodes = [X.NodeContent (T.pack $ show i)]
                 put resNodes
                 lift $ put $ X.Element (makeName $ T.pack "PrimInt") M.empty resNodes
    , writeInteger =
          \i ->
              do let resNodes = [X.NodeContent (T.pack $ show i)]
                 put resNodes
                 lift $ put $ X.Element (makeName $ T.pack "PrimInteger") M.empty resNodes
    , writeString =
          \s ->
              do let resNodes = [X.NodeContent (T.pack s)]
                 put resNodes
                 lift $ put $ X.Element (makeName $ T.pack "PrimString") M.empty resNodes
    , writeDouble =
          \d ->
               do let resNodes = [X.NodeContent (T.pack $ show d)]
                  put resNodes
                  lift $ put $ X.Element (makeName $ T.pack "PrimDouble") M.empty resNodes
    , writeBool =
          \b ->
               do let resNodes = [X.NodeContent (T.pack $ show b)]
                  put resNodes
                  lift $ put $ X.Element (makeName $ T.pack "PrimBool") M.empty resNodes
    , writeChar =
          \c ->
               do let resNodes = [X.NodeContent (T.pack $ show c)]
                  put resNodes
                  lift $ put $ X.Element (makeName $ T.pack "PrimChar") M.empty resNodes
    , writeRepetition =
          \ar _ ->
              do let value = X.Element (makeName (T.pack "value")) M.empty []
                     arrAttr = M.fromList [("type", "array")]
                 case ar of
                   [] ->
                       do put [X.NodeElement value]
                          lift $ put $ X.Element (makeName "PrimList") arrAttr []
                          return ()
                   h:t ->
                       do lift $ put value
                          putter <- getSmartPut sFormat
                          unversHead <- lift get
                          put [X.NodeElement unversHead]
                          withField sFormat $ putter h
                          versHead <- get
                          put $ replicate (length ar - 1) (X.NodeElement value)
                          forM_ t $ \a ->
                               do withField sFormat $ writeSmart sFormat a Nothing
                                  field <- get
                                  put field
                          res <- get
                          put $ versHead++res
                          lift $ put $ X.Element (makeName "PrimList") arrAttr (versHead++res)
    , writeMaybe =
          \ma _ ->
              case ma of
                Just a ->
                    do let optAttr = M.fromList [("type", "opt")]
                       putter <- getSmartPut sFormat
                       putter a
                       el <- lift get
                       lift $ put $ X.Element (X.elementName el) optAttr (X.elementNodes el)
                Nothing ->
                    do put []
                       return ()
    , writeBS =
          \bs ->
               do let resNodes = [X.NodeContent (TE.decodeUtf8 bs)]
                  put resNodes
                  lift $ put $ X.Element (makeName $ T.pack "PrimByteString") M.empty resNodes
    , writeText =
          \text ->
               do let resNodes = [X.NodeContent text]
                  put resNodes
                  lift $ put $ X.Element (makeName $ T.pack "PrimText") M.empty resNodes
    }
                     
-------------------------------------------------------------------------------
-- Versioned parsing
-------------------------------------------------------------------------------

pFormat :: ParseFormat (FailT (StateT [X.Node] (StateT X.Element (State [X.Node]))))
pFormat
    = ParseFormat
    { mkGetter =
          \_ _ prevVers ->
              do nodeElems <- get
                 case nodeElems of
                   [] ->
                       return $ readSmart pFormat
                   _ ->
                       do el <- pop
                          case el of
                            X.NodeContent _ ->
                                return $ readSmart pFormat
                            X.NodeElement el ->
                                do vers <- readVersion el
                                   case prevVers of
                                     Just p ->
                                         case constructGetterFromVersion pFormat (Version p) kind of
                                           Right getter ->
                                               return getter
                                           Left msg ->
                                               fail msg
                                     Nothing ->
                                         case vers of
                                           Just v ->
                                               case constructGetterFromVersion pFormat v kind of
                                                 Right getter ->
                                                     return getter
                                                 Left msg ->
                                                     fail msg
                                           Nothing -> 
                                               return $ readSmart pFormat
                            _ ->
                                return $ mismatch "NodeContent or NodeElement" (show nodeElems)
    , readCons =
          \cons ->
              do elem <- lift $ lift get
                 let conNames = map (cname . fst) cons
                     parsers = map snd cons
                 case cons of
                   [] -> noCons
                   _ -> 
                       do let con = elName elem
                          case lookup con (zip conNames parsers) of
                            Just parser ->
                                do lift $ lift $ lift $ put $ X.elementNodes elem
                                   put $ X.elementNodes elem
                                   parser
                            _ -> conLookupErr (show con) (show conNames)
    , readField =
          \ma ->
              do nodes <- lift $ lift $ lift get
                 case nodes of
                   [] -> ma
                   (x:xs) -> 
                       case x of
                         X.NodeElement elem ->
                             do case X.elementNodes elem of
                                  [] ->
                                      lift $ lift $ lift $ put []
                                  [X.NodeContent _] ->
                                      lift $ lift $ lift $ put $ X.elementNodes elem
                                  e@[X.NodeElement elem'] ->
                                      case M.lookup "type" (X.elementAttributes elem) of
                                        Just "array" ->
                                            case X.elementNodes elem' of
                                              [] -> 
                                                  do lift $ lift $ lift $ put []
                                                     return ()
                                              _ ->
                                                  do lift $ lift $ put elem'
                                                     lift $ lift $ lift $ put e
                                        Just "opt" ->
                                            do lift $ lift $ put elem'
                                               lift $ lift $ lift $ put e
                                        Nothing ->
                                            lift $ lift $ put elem'
                                  nelems ->
                                      lift $ lift $ lift $ put nelems
                                res <- ma
                                lift $ lift $ lift $ put xs
                                return res
                         _ ->
                             mismatch "NodeElement" (show x)
    , readRepetition =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case nodes of
               [] ->
                   case el' of
                     X.Element (X.Name "PrimList" _ _) _ nodes' ->
                         do lift $ lift $ lift $ put nodes'
                            readListVals nodes'
                     _ -> return []
               _ -> readListVals nodes
    , readInt =
          do nodes <- lift $ lift $ lift get
             -- Handles primitive values at top-level, wraps vals in XML-elements
             el' <- lift $ lift get
             case nodes of
               [] ->
                   case el' of
                     X.Element (X.Name "PrimInt" _ _) _ nodes ->
                         getIntContent nodes
                     _ ->
                         mismatch "primitive int at toplevel" (show el')
               _ -> getIntContent nodes
    , readDouble =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case nodes of
               [] ->
                   case el' of
                     X.Element (X.Name "PrimDouble" _ _) _ nodes ->
                         getDoubleContent nodes
                     _ ->
                         mismatch "primitive double at toplevel" (show el')
               _ -> getDoubleContent nodes
    , readBool =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case nodes of
               [] ->
                   case el' of
                     X.Element (X.Name "PrimBool" _ _) _ nodes ->
                        getBoolContent nodes
                     _ -> mismatch "primitive bool at toplevel" (show el')
               _ -> getBoolContent nodes
    , readString =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case nodes of
               [] ->
                   case el' of
                     X.Element (X.Name "PrimString" _ _) _ nodes ->
                         getStringContent nodes
                     _ -> mismatch "primitive string at toplevel" (show el')
               _ -> getStringContent nodes
    , readChar =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case nodes of
               [] ->
                   case el' of
                     X.Element (X.Name "PrimChar" _ _) _ nodes ->
                         getCharContent nodes
                     _ -> mismatch "primitive char at toplevel" (show el')
               _ -> getCharContent nodes
    , readMaybe =
          do nodes <- lift $ lift $ lift get
             case nodes of
               [] -> return Nothing
               X.NodeElement el:xs ->
                   case X.elementNodes el of
                     [] -> return Nothing
                     elNodes ->
                         do put nodes
                            lift $ lift $ lift $ put xs
                            getSmartGet pFormat >>= liftM Just
               cont@[X.NodeContent x] ->
                   do put cont
                      getSmartGet pFormat >>= liftM Just 
               h:_ -> mismatch "Maybe node" (show h)
    , readBS =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case nodes of
               [] ->
                   case el' of
                     X.Element (X.Name "PrimByteString" _ _) _ nodes ->
                         liftM BSC.pack $ getStringContent nodes
                     _ -> mismatch "primitive char at toplevel" (show el')
               _ -> liftM BSC.pack $ getStringContent nodes
    , readText =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case nodes of
               [] ->
                   case el' of
                     X.Element (X.Name "PrimText" _ _) _ nodes ->
                         liftM T.pack $ getStringContent nodes
                     _ -> mismatch "primitive char at toplevel" (show el')
               _ -> liftM T.pack $ getStringContent nodes
    }
    where getIntContent (n:nodes) =
              case n of
                X.NodeContent t ->
                    case reads (T.unpack t) of
                      [(int, [])] -> return int
                      _ -> mismatch "Int" (show n)
                _ -> mismatch "NodeContent" (show n)
          getDoubleContent (n:nodes) =
              case n of
                X.NodeContent t ->
                    case reads (T.unpack t) of
                      [(double, [])] -> return double
                      _ -> mismatch "Double" (show n)
                _ -> mismatch "NodeContent" (show n)
          getBoolContent (n:nodes) =
              case n of
                X.NodeContent t
                    | T.unpack t == "True" -> return True
                    | T.unpack t == "False" -> return False
                    | otherwise -> mismatch "Bool" (show t)
                _ -> mismatch "NodeContent" (show n)
          getStringContent (n:nodes) =
              case n of
                X.NodeContent t ->
                    return $ T.unpack t
                _ -> mismatch "NodeContent" (show n)
          getCharContent (n:nodes) =
              case n of
                X.NodeContent t ->
                    case T.unpack t of
                      [c] -> return c
                      _ -> mismatch "Char" (show t)
                _ -> mismatch "NodeContent" (show n)
          readListVals :: SmartCopy a
                       => [X.Node]
                       -> FailT (StateT [X.Node] (StateT X.Element (State [X.Node]))) [a]
          readListVals nodes =
              do put nodes
                 getSmartGet pFormat >>= (replicateM (length nodes) . readField pFormat)


-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

emptyEl = X.Element (X.Name T.empty Nothing Nothing) M.empty []

makeName t = X.Name t Nothing Nothing

elName = X.nameLocalName . X.elementName

makeEmptyNode text = X.NodeElement $ X.Element (makeName text) M.empty []

pop =
    do nodes <- get
       case nodes of
         [] -> return $ X.NodeElement emptyEl
         h:t -> do { put t; return h }

readVersion el =
    do let attribs = map ((X.nameLocalName . fst) &&& snd)
                         (M.toList $ X.elementAttributes el)
           vers = lookup (T.pack "version") attribs 
       case vers of
         Just vText ->
             case (reads . T.unpack) vText of
               [(int,[])] -> return $ Just $ Version int
               _ -> mismatch "int32 for version" (show vText)
         Nothing -> return Nothing
