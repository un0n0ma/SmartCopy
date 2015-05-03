{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import SmartCopy

mkMigrateFun :: [(Integer, Con)] -> DecQ
mkMigrateFun cons = funD 'migrateFwd $ map mkFwdClause cons
    where
        mkFwdClause = undefined
    
