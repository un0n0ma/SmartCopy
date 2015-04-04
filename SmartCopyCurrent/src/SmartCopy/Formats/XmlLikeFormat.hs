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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC (pack, unpack)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Text.XML as X

import Data.String.Utils
--import Data.String.UTF8 hiding (length)

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer

import Control.Applicative
import Control.Arrow
import Control.Monad.Loops
import Data.Either (lefts, rights)


-------------------------------------------------------------------------------
-- Run functions, versioned and unversioned
-------------------------------------------------------------------------------

serializeSmart a
    = TL.unpack $ X.renderText X.def $
                  X.Document (X.Prologue [] Nothing [])
                 (runSerialization (smartPut sFormat a)) []
    where runSerialization m
                = execState (evalStateT m []) emptyEl

parseSmart :: SmartCopy a => String -> Fail a
parseSmart
    = runParser (fromEitherM $ smartGet pFormat)
    where runParser action value =
              do let (X.Document _ el _) = X.parseText_ X.def (TL.pack value)
                 evalState (evalStateT (evalStateT (runFailT action) [X.NodeElement el]) el) []

serializeUnvers a
    = TL.unpack $ X.renderText X.def $
                    X.Document (X.Prologue [] Nothing [])
                    (runSerialization (writeSmart sFormatUnvers a)) []
    where runSerialization m = execState (evalStateT m []) emptyEl

parseUnvers :: SmartCopy a => String -> Fail a
parseUnvers
    = runParser (fromEitherM $ readSmart pFormatUnvers)
    where runParser action value =
              do let (X.Document _ el _) = X.parseText_ X.def (TL.pack value)
                 evalState (evalStateT (evalStateT (runFailT action) [X.NodeElement el]) el) []

-------------------------------------------------------------------------------
-- Xml serialization unversioned
-------------------------------------------------------------------------------

sFormatUnvers
    = sFormat
    { mkPutter = \_ v -> return $ writeSmart sFormatUnvers
    , writeRepetition =
          \ar ->
              do let arrAttr = M.fromList [("type", "array")]
                 let value = X.Element (makeName (T.pack "value")) M.empty []
                 case length ar of
                   0 -> do put [X.NodeElement value]
                           lift $ put $ X.Element (makeName "PrimList") arrAttr []
                           return ()
                   n -> do let value = X.Element (makeName (T.pack "value")) M.empty []
                           put $ replicate n (X.NodeElement value)
                           forM_ ar $ \a ->
                               do withField sFormatUnvers $ writeSmart sFormatUnvers a
                                  field <- get
                                  put field
                           res <- get
                           put res
                           lift $ put $ X.Element (makeName "PrimList") arrAttr res
    , writeMaybe =
          \ma ->
              case ma of
                Just a ->
                    do let optAttr = M.fromList [("type", "opt")]
                       writeSmart sFormatUnvers a
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
    { mkGetter = \_ _ -> return $ readSmart pFormatUnvers
    , readRepetition =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case length nodes of
               0 ->
                   case el' of
                     X.Element (X.Name "PrimList" _ _) _ nodes' ->
                         do lift $ lift $ lift $ put nodes'
                            res <-
                                replicateM (length nodes') (readField pFormatUnvers $
                                readSmart pFormatUnvers)
                            if null (lefts res)
                               then return $ Right $ rights res
                               else return $ Left []
                     _ -> return $ Right []
               n ->
                   do res <- replicateM n (readField pFormatUnvers $ readSmart pFormatUnvers)
                      if null (lefts res)
                         then return $ Right $ rights res
                         else return $ Left []
    , readMaybe =
          do nodes <- lift $ lift $ lift get
             case nodes of
               [] -> return $ Right Nothing
               X.NodeElement el:xs ->
                   case X.elementNodes el of
                     [] -> return $ Right Nothing
                     elNodes ->
                         do put elNodes
                            lift $ lift $ lift $ put xs
                            liftM (fmap Just) $ readSmart pFormatUnvers
               cont@[X.NodeContent val] ->
                   do put cont
                      liftM (fmap Just) $ readSmart pFormatUnvers
               h:_ -> mismatch "Maybe node" (show h)
    }

    
-------------------------------------------------------------------------------
-- Xml serialization versioned
-------------------------------------------------------------------------------

sFormat :: SerializationFormat (StateT [X.Node] (State X.Element))
sFormat
    = SerializationFormat
    { mkPutter =
          \_ ver ->
              return $ \a ->
                  do writeSmart sFormat a
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
                          lift $ put $ 
                              X.Element (X.elementName resVers)
                                        (X.elementAttributes resVers) resNodes
                   _ -> mismatchFail "NodeElement" (show nodes)
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
          \ar ->
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
                               do withField sFormat $ writeSmart sFormat a
                                  field <- get
                                  put field
                          res <- get
                          put $ versHead++res
                          lift $ put $ X.Element (makeName "PrimList") arrAttr (versHead++res)
    , writeMaybe =
          \ma ->
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
    where makeEmptyNode text = X.NodeElement $ X.Element (makeName text) M.empty []
                     
-------------------------------------------------------------------------------
-- Versioned parsing
-------------------------------------------------------------------------------

pFormat :: ParseFormat (FailT (StateT [X.Node] (StateT X.Element (State [X.Node]))))
pFormat
    = ParseFormat
    { mkGetter =
          \_ prevVer ->
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
                                   case vers of
                                     Right (Just v) ->
                                         case constructGetterFromVersion pFormat v kind of
                                           Right getter ->
                                               return getter
                                           Left msg ->
                                               return $ return $ Left msg
                                     Right Nothing ->
                                         return $ readSmart pFormat
                                     Left msg -> fail msg
                            _ ->
                                return $ mismatch "NodeContent or NodeElement" (show nodeElems)
    , withLookahead =
          \_ ma mb ->
              do consumed <- lift $ lift get
                 res <- ma
                 case res of
                   Left _ ->
                       lift (lift $ put consumed) >> mb
                   r@(Right _) ->
                       return r
    , readCons =
          \cons ->
              do elem <- lift $ lift get
                 let conNames = map (cname . fst) cons
                     conFields = map (cfields . fst) cons
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
                     _ -> return $ Right []
               _ -> readListVals nodes
    , readInt =
          do nodes <- lift $ lift $ lift get
             -- Handles primitive values at toplevel, wraps vals in XML-elements
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
               [] -> return $ Right Nothing
               X.NodeElement el:xs ->
                   case X.elementNodes el of
                     --  TODO: Not sure if this is correct
                     [] -> return $ Right Nothing
                     elNodes ->
                         do put nodes
                            lift $ lift $ lift $ put xs
                            getSmartGet pFormat >>= liftM (fmap Just)
               cont@[X.NodeContent x] ->
                   do put cont
                      getSmartGet pFormat >>= liftM (fmap Just)
               h:_ -> mismatch "Maybe node" (show h)
    , readBS =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case nodes of
               [] ->
                   case el' of
                     X.Element (X.Name "PrimByteString" _ _) _ nodes ->
                         liftM (fmap BSC.pack) $ getStringContent nodes
                     _ -> mismatch "primitive char at toplevel" (show el')
               _ -> liftM (fmap BSC.pack) $ getStringContent nodes
    , readText =
          do nodes <- lift $ lift $ lift get
             el' <- lift $ lift get
             case nodes of
               [] ->
                   case el' of
                     X.Element (X.Name "PrimText" _ _) _ nodes ->
                         liftM (fmap T.pack) $ getStringContent nodes
                     _ -> mismatch "primitive char at toplevel" (show el')
               _ -> liftM (fmap T.pack) $ getStringContent nodes
    }
    where getIntContent (n:nodes) =
              case n of
                X.NodeContent t ->
                    case reads (T.unpack t) of
                      [(int, [])] -> return $ Right int
                      _ -> mismatch "Int" (show n)
                _ -> mismatch "NodeContent" (show n)
          getDoubleContent (n:nodes) =
              case n of
                X.NodeContent t ->
                    case reads (T.unpack t) of
                      [(double, [])] -> return $ Right double
                      _ -> mismatch "Double" (show n)
                _ -> mismatch "NodeContent" (show n)
          getBoolContent (n:nodes) =
              case n of
                X.NodeContent t
                    | T.unpack t == "True" -> return $ Right True
                    | T.unpack t == "False" -> return $ Right False
                    | otherwise -> mismatch "Bool" (show t)
                _ -> mismatch "NodeContent" (show n)
          getStringContent (n:nodes) =
              case n of
                X.NodeContent t ->
                    return $ Right $ T.unpack t
                _ -> mismatch "NodeContent" (show n)
          getCharContent (n:nodes) =
              case n of
                X.NodeContent t ->
                    case T.unpack t of
                      [c] -> return $ Right c
                      _ -> mismatch "Char" (show t)
                _ -> mismatch "NodeContent" (show n)
          readListVals nodes =
              do put nodes
                 res <-
                     getSmartGet pFormat >>= (replicateM (length nodes) . readField pFormat)
                 if null (lefts res)
                    then return $ Right $ rights res
                    else return $ Left []
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
                         [(int,[])] -> return $ Right $ Just $ Version int
                         _ -> mismatch "int32 for version" (show vText)
                   Nothing -> return $ Right Nothing


-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

emptyEl = X.Element (X.Name T.empty Nothing Nothing) M.empty []

makeName t = X.Name t Nothing Nothing

elName = X.nameLocalName . X.elementName
