{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.String.Utils

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Control.Applicative
import Control.Arrow
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

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
              case length ar of
                0 -> return ()
                n -> do let value = X.Element (makeName (T.pack "value")) M.empty []
                        put $ replicate n (X.NodeElement value)
                        forM_ ar $ \a -> do withField sFormatUnvers $ writeSmart sFormatUnvers a
                                            field <- get
                                            put field
                        res <- get
                        put res
                        lift $ put $ X.Element (makeName "values") M.empty res
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
             replicateM (length nodes) (readField pFormatUnvers $ readSmart pFormatUnvers)
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
                    put [X.NodeContent (T.pack $ show i)]
                _ -> mismatch "Prim Int" (show prim)
    , writeInteger =
          \prim ->
              case prim of
                PrimInteger i ->
                    put [X.NodeContent (T.pack $ show i)]
                _ -> mismatch "Prim Integer" (show prim)
    , writeString =
          \prim ->
              case prim of
                PrimString s ->
                    put [X.NodeContent (T.pack s)]
                _ -> mismatch "Prim String" (show prim)
    , writeDouble =
          \prim ->
              case prim of
                PrimDouble d ->
                    put [X.NodeContent (T.pack $ show d)]
                _ -> mismatch "Prim Double" (show prim)
    , writeBool =
          \prim ->
              case prim of
                PrimBool b ->
                    put [X.NodeContent (T.pack $ show b)]
                _ -> mismatch "Prim Bool" (show prim)
    , writeChar =
          \prim ->
              case prim of
                PrimChar c ->
                    put [X.NodeContent (T.pack $ show c)]
                _ -> mismatch "Prim Char" (show prim)
    , writeRepetition =
          \ar ->
              case length ar of
                0 -> return ()
                n -> do let value = X.Element (makeName (T.pack "value")) M.empty []
                        lift $ put value
                        putter <- getSmartPut sFormat
                        unversHead <- lift get
                        put [X.NodeElement unversHead]
                        withField sFormat $ putter (head ar)
                        versHead <- get
                        put $ replicate (n-1) (X.NodeElement value)
                        forM_ (tail ar) $ \a -> do withField sFormat $ writeSmart sFormat a
                                                   field <- get
                                                   put field
                        res <- get
                        put $ versHead++res
                        lift $ put $ X.Element (makeName "values") M.empty (versHead++res)
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
             put nodes
             getSmartGet pFormat >>= (replicateM (length nodes) . readField pFormat)
    , readInt =
          do nodes <- lift $ lift $ lift get
             case length nodes of
               0 -> mismatch "Int NodeContent" (show nodes)
               n ->
                   case head nodes of
                     X.NodeContent t ->
                         case reads (T.unpack t) of
                           [(int, [])] -> return $ PrimInt int
                           _ -> mismatch "Int" (show $ head nodes)
                     _ -> mismatch "NodeContent" (show $ head nodes)
    , readDouble =
          do nodes <- lift $ lift $ lift get
             case length nodes of
               0 -> mismatch "Double NodeContent" (show nodes)
               n ->
                   case head nodes of
                     X.NodeContent t ->
                         case reads (T.unpack t) of
                           [(double, [])] -> return $ PrimDouble double
                           _ -> mismatch "Double" (show $ head nodes)
                     _ -> mismatch "NodeContent" (show $ head nodes)
    , readBool =
          do nodes <- lift $ lift $ lift get
             case length nodes of
               0 -> mismatch "Bool NodeContent" (show nodes)
               n ->
                   case head nodes of
                     X.NodeContent t
                         | T.unpack t == "True" -> return $ PrimBool True
                         | T.unpack t == "False" -> return $ PrimBool False
                         | otherwise -> mismatch "Bool" (show t)
                     _ -> mismatch "NodeContent" (show $ head nodes)
    , readString =
          do nodes <- lift $ lift $ lift get
             case length nodes of
               0 -> mismatch "String NodeContent" (show nodes)
               n ->
                   case head nodes of
                     X.NodeContent t ->
                         return $ PrimString (T.unpack t)
                     _ -> mismatch "NodeContent" (show $ head nodes)
    , readChar =
          do nodes <- lift $ lift $ lift get
             case length nodes of
               0 -> mismatch "Char NodeContent" (show nodes)
               n ->
                   case head nodes of
                     X.NodeContent t ->
                         if T.length t == 1
                            then return $ PrimChar $ T.head t
                            else mismatch "Char" (show t)
                     _ -> mismatch "NodeContent" (show $ head nodes)
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


-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

emptyEl = X.Element (X.Name T.empty Nothing Nothing) M.empty []
makeName t = X.Name t Nothing Nothing
elName = X.nameLocalName . X.elementName

