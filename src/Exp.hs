------------------------------------------------------------------
-- 
--
-- Created: 8-2-2018
--
------------------------------------------------------------------

module Exp where

import Domain.Refactoring.Strategy
import Domain.Refactoring.Util
import Domain.Parsers.JavaParser
import Domain.Syntax
import Domain.Printer
import Utils.Utils
import Utils.FileUtils
import Domain.Evaluator

import Ideas.Common.Library as Ideas

import Control.Monad

-- Removed??
makeEx :: Strategy (Context Program) -> Exercise Program
makeEx s = undefined

test1 = fmap (allDerivations (makeEx norm)) example1SP1
testDerivF f = f >>= printDerivation (makeEx norm)

testAllDerivF f = do
    p <- f
    let derivs = allDerivations (makeEx norm) p
    return (map steps derivs)

-- testStrategy :: Program -> JavaCode
testStrategy p = do
    let x = applyAll norm . newTermContext $ p
    y <- mapM fromContext x
    fmap ppJavaProgram y

----- Exercise SumPos

-- | Model program with method
example1MPMethod :: IO Program
example1MPMethod = forceReadMethod "../test/rpt/exSum/pmp.java"

-- | Student programs
example1SP1, example1SP2 :: IO Program
example1SP1 = forceReadMethod "../test/rpt/exSum/sp1.java" 
example1SP2 = forceReadMethod "../test/rpt/exSum/sp1.java"
              
-- | Test programs
example1TP1, example1TP2 :: IO Program
example1TP1 = forceReadMethod "../test/rpt/exSum/tp1.java"
example1TP2 = forceReadMethod "../test/rpt/exSum/tp2.java"

model1 = "C:/Users/p43341961/Dropbox/Onderzoek/Afst/Research/Validation/Analyse/Java 1/1"
loadF = "C:/Users/p43341961/Dropbox/Onderzoek/RPT/Data/Uitwerkingen Fib"
