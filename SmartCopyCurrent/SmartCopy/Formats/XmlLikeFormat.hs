{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SmartCopy.Formats.XmlLikeFormat
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
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.XML as X

import Data.String.UTF8 hiding (length)

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer

import Control.Applicative
import Control.Arrow
import Control.Monad.Loops

import Data.Either (rights)


-------------------------------------------------------------------------------
-- Run functions, versioned and unversioned
-------------------------------------------------------------------------------

serializeSmart a = TL.unpack $ X.renderText X.def $
                   X.Document (X.Prologue [] Nothing [])
                   (runSerialization (smartPut sFormat a)) []
    where runSerialization m
                = execState (evalStateT m []) emptyEl

parseSmart :: SmartCopy a => String -> Fail a
parseSmart = runParser (smartGet pFormat)
    where runParser action value =
              do let (X.Document _ el _) = X.parseText_ X.def (TL.pack value)
                 evalState (evalStateT (evalStateT (runFailT action) [X.NodeElement el]) el) []

serializeUnvers a = TL.unpack $ X.renderText X.def $
                    X.Document (X.Prologue [] Nothing [])
                    (runSerialization (writeSmart sFormatUnvers a)) []
    where runSerialization m = execState (evalStateT m []) emptyEl

parseUnvers :: SmartCopy a => String -> Fail a
parseUnvers = runParser (readSmart pFormatUnvers)
    where runParser action value =
              do let (X.Document _ el _) = X.parseText_ X.def (TL.pack value)
                 evalState (evalStateT (evalStateT (runFailT action) [X.NodeElement el]) el) []

-------------------------------------------------------------------------------
-- Xml serialization unversioned
-------------------------------------------------------------------------------

sFormatUnvers
    = sFormat
    { writeVersion = \_ -> return ()
    , withVersion = const id
    , writeRepetition =
          \ar ->
              do let value = X.Element (makeName (T.pack "value")) M.empty []
                 case length ar of
                   0 -> do put [X.NodeElement value]
                           lift $ put $ X.Element (makeName "PrimList") M.empty []
                           return ()
                   n -> do let value = X.Element (makeName (T.pack "value")) M.empty []
                           put $ replicate n (X.NodeElement value)
                           forM_ ar $ \a -> do withField sFormatUnvers $ writeSmart sFormatUnvers a
                                               field <- get
                                               put field
                           res <- get
                           put res
                           lift $ put $ X.Element (makeName "PrimList") M.empty res
    , writeMaybe =
          \ma ->
              case ma of
                Just a ->
                    writeSmart sFormatUnvers a
                Nothing ->
                    do put []
                       return ()
    }


-------------------------------------------------------------------------------
-- Xml parsing unversioned
-------------------------------------------------------------------------------

pFormatUnvers
    = pFormat
    { readVersioned = id
    , readVersion = return Nothing
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
                     _ -> mismatch "primitive list at top-level" (show el')
               n -> replicateM (length nodes) (readField pFormatUnvers $ readSmart pFormatUnvers)
    , readMaybe =
          do nodes <- lift $ lift $ lift get
             case nodes of
               [] -> mismatch "field nodes" (show nodes)
               X.NodeElement el:xs ->
                   case X.elementNodes el of
                     [] -> return Nothing
                     elNodes ->
                         do put elNodes
                            lift $ lift $ lift $ put xs
                            liftM Just $ readSmart pFormatUnvers
               _ -> mismatch "element node containing Maybe value" (show $ head nodes)
    }

    
-------------------------------------------------------------------------------
-- Xml serialization versioned
-------------------------------------------------------------------------------

sFormat :: SerializationFormat (StateT [X.Node] (State X.Element))
sFormat
    = SerializationFormat
    { writeVersion =
          \ver ->
          do resEl <- lift get
             let versEl = resEl
                        { X.elementAttributes
                        = M.insert (makeName (T.pack "version")) (T.pack $ show $ unVersion ver)
                                   (X.elementAttributes resEl)}
             lift $ put versEl
    , withVersion =
          \ver ma ->
          do ma
             writeVersion sFormat ver
    , withCons =
          \cons ma ->
          do let fields =
                      case cfields cons of
                        Empty -> []
                        NF i -> map (T.pack . show) [0..i-1]
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
                      lift $ put $ X.Element (X.elementName resVers) (X.elementAttributes resVers) resNodes
               _ -> mismatch "NodeElement" (show nodes)
    , writeInt =
          \prim ->
              case prim of
                PrimInt i ->
                    do let resNodes = [X.NodeContent (T.pack $ show i)]
                       put resNodes
                       lift $ put $ X.Element (makeName $ T.pack "PrimInt") M.empty resNodes
                _ -> mismatch "Prim Int" (show prim)
    , writeInteger =
          \prim ->
              case prim of
                PrimInteger i ->
                    do let resNodes = [X.NodeContent (T.pack $ show i)]
                       put resNodes
                       lift $ put $ X.Element (makeName $ T.pack "PrimInteger") M.empty resNodes
                _ -> mismatch "Prim Integer" (show prim)
    , writeString =
          \prim ->
              case prim of
                PrimString s ->
                    do let resNodes = [X.NodeContent (T.pack s)]
                       put resNodes
                       lift $ put $ X.Element (makeName $ T.pack "PrimString") M.empty resNodes
                _ -> mismatch "Prim String" (show prim)
    , writeDouble =
          \prim ->
              case prim of
                PrimDouble d ->
                    do let resNodes = [X.NodeContent (T.pack $ show d)]
                       put resNodes
                       lift $ put $ X.Element (makeName $ T.pack "PrimDouble") M.empty resNodes
                _ -> mismatch "Prim Double" (show prim)
    , writeBool =
          \prim ->
              case prim of
                PrimBool b ->
                    do let resNodes = [X.NodeContent (T.pack $ show b)]
                       put resNodes
                       lift $ put $ X.Element (makeName $ T.pack "PrimBool") M.empty resNodes
                _ -> mismatch "Prim Bool" (show prim)
    , writeChar =
          \prim ->
              case prim of
                PrimChar c ->
                    do let resNodes = [X.NodeContent (T.pack $ show c)]
                       put resNodes
                       lift $ put $ X.Element (makeName $ T.pack "PrimChar") M.empty resNodes
                _ -> mismatch "Prim Char" (show prim)
    , writeRepetition =
          \ar ->
              do let value = X.Element (makeName (T.pack "value")) M.empty []
                 case length ar of
                   0 -> do put [X.NodeElement value]
                           lift $ put $ X.Element (makeName "PrimList") M.empty []
                           return ()
                   n -> do lift $ put value
                           putter <- getSmartPut sFormat
                           unversHead <- lift get
                           put [X.NodeElement unversHead]
                           withField sFormat $ putter (head ar)
                           versHead <- get
                           put $ replicate (n-1) (X.NodeElement value)
                           forM_ (tail ar) $ \a ->
                                do withField sFormat $ writeSmart sFormat a
                                   field <- get
                                   put field
                           res <- get
                           put $ versHead++res
                           lift $ put $ X.Element (makeName "PrimList") M.empty (versHead++res)
    , writeMaybe =
          \ma ->
              case ma of
                Just a ->
                    do putter <- getSmartPut sFormat
                       putter a
                Nothing ->
                    do put []
                       return ()
    }
    where makeEmptyNode text = X.NodeElement $ X.Element (makeName text) M.empty []
                     
-------------------------------------------------------------------------------
-- Versioned parsing
-------------------------------------------------------------------------------

pFormat :: ParseFormat (FailT (StateT [X.Node] (StateT X.Element (State [X.Node]))))
pFormat
    = ParseFormat
    { readVersioned = id
    , readVersion =
          do nodeElems <- get
             case length nodeElems of
               0 -> return $ Just $ Version 0
               n ->
                   do let nodeElem = head nodeElems
                      case nodeElem of
                        X.NodeContent _ ->
                            do put $ tail nodeElems
                               return $ Just $ Version 0
                        X.NodeElement el -> 
                               do let attribs = map ((X.nameLocalName . fst) &&& snd)
                                                (M.toList $ X.elementAttributes el)
                                      vers = lookup (T.pack "version") attribs
                                  case vers of
                                    Just vText ->
                                        case (reads . T.unpack) vText of
                                          [(int,[])] ->
                                              do put $ tail nodeElems
                                                 return $ Just $ Version int
                                          _ -> mismatch "int32 for version" (show vText)
                                    Nothing ->
                                        return $ Just $ Version 0
                        _ -> mismatch "NodeContent or NodeElement" (show nodeElem)
    , readCons =
          \cons ->
              do elem <- lift $ lift get
                 let conNames = map (cname . fst) cons
                     conFields = map (cfields . fst) cons
                     parsers = map snd cons
                 case length cons of
                   0 -> noCons
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
                                  [] -> return ()
                                  [X.NodeContent _] ->
                                      lift $ lift $ lift $ put $ X.elementNodes elem
                                  [X.NodeElement elem'] ->
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
             case length nodes of
               0 ->
                   case el' of
                     X.Element (X.Name "PrimList" _ _) _ nodes' ->
                         do lift $ lift $ lift $ put nodes'
                            readListVals nodes'
                     _ -> mismatch "primitive list at top-level" (show el')
               n -> readListVals nodes
    , readInt =
          do nodes <- lift $ lift $ lift get
             -- Handles primitive values at toplevel (wrapped in noname-element)
             el' <- lift $ lift get
             case length nodes of
               0 ->
                   case el' of
                     X.Element (X.Name "PrimInt" _ _) _ nodes ->
                         getIntContent nodes
                     _ ->
                         mismatch "primitive int at toplevel" (show el')
               n -> getIntContent nodes
    , readDouble =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case length nodes of
               0 ->
                   case el' of
                     X.Element (X.Name "PrimDouble" _ _) _ nodes ->
                         getDoubleContent nodes
                     _ ->
                         mismatch "primitive double at toplevel" (show el')
               n -> getDoubleContent nodes
    , readBool =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case length nodes of
               0 ->
                   case el' of
                     X.Element (X.Name "PrimBool" _ _) _ nodes ->
                        getBoolContent nodes
                     _ -> mismatch "primitive bool at toplevel" (show el')
               n -> getBoolContent nodes
    , readString =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case length nodes of
               0 ->
                   case el' of
                     X.Element (X.Name "PrimString" _ _) _ nodes ->
                         getStringContent nodes
                     _ -> mismatch "primitive string at toplevel" (show el')
               n -> getStringContent nodes
    , readChar =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case length nodes of
               0 ->
                   case el' of
                     X.Element (X.Name "PrimChar" _ _) _ nodes ->
                         getCharContent nodes
                     _ -> mismatch "primitive char at toplevel" (show el')
               n -> getCharContent nodes
    , readMaybe =
          do nodes <- lift $ lift $ lift get
             case nodes of
               [] -> mismatch "field nodes" (show nodes)
               X.NodeElement el:xs ->
                   case X.elementNodes el of
                     [] -> return Nothing
                     elNodes ->
                         do put nodes
                            lift $ lift $ lift $ put xs
                            getSmartGet pFormat >>= liftM Just
               _ -> mismatch "element node containing Maybe value" (show $ head nodes)
    }
    where getIntContent nodes =
              case head nodes of
                X.NodeContent t ->
                    case reads (T.unpack t) of
                      [(int, [])] -> return $ PrimInt int
                      _ -> mismatch "Int" (show $ head nodes)
                _ -> mismatch "NodeContent" (show $ head nodes)
          getDoubleContent nodes =
              case head nodes of
                X.NodeContent t ->
                    case reads (T.unpack t) of
                      [(double, [])] -> return $ PrimDouble double
                      _ -> mismatch "Double" (show $ head nodes)
                _ -> mismatch "NodeContent" (show $ head nodes)
          getBoolContent nodes =
                   case head nodes of
                     X.NodeContent t
                         | T.unpack t == "True" -> return $ PrimBool True
                         | T.unpack t == "False" -> return $ PrimBool False
                         | otherwise -> mismatch "Bool" (show t)
                     _ -> mismatch "NodeContent" (show $ head nodes)
          getStringContent nodes =
                   case head nodes of
                     X.NodeContent t ->
                         return $ PrimString (T.unpack t)
                     _ -> mismatch "NodeContent" (show $ head nodes)
          getCharContent nodes =
                   case head nodes of
                     X.NodeContent t ->
                         if T.length t == 1
                            then return $ PrimChar $ T.head t
                            else mismatch "Char" (show t)
                     _ -> mismatch "NodeContent" (show $ head nodes)
          readListVals nodes =
                   do put nodes
                      getSmartGet pFormat >>= (replicateM (length nodes) . readField pFormat)



-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

emptyEl = X.Element (X.Name T.empty Nothing Nothing) M.empty []
makeName t = X.Name t Nothing Nothing
elName = X.nameLocalName . X.elementName

