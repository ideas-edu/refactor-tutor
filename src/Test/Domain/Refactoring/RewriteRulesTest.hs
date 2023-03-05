{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Domain.Refactoring.RewriteRulesTest where

import Test.TestUtils
import Test.Framework
import Test.Framework.TestInterface (Assertion)
import Domain.Refactoring.Rules.Rules
import Domain.Refactoring.Rules.LoopRules
import Domain.Transformation.ProgramTransformation
import Domain.Base.Normalisation
import Domain.Syntax
import Domain.Parsers.JavaParser
import Ideas.Common.Library 

-- Blocks --------------------------------------------------

test_removeEmptyStats,
    test_removeEmptyBlock, test_removeEmptyBlock2,
    test_removeSingleBlock, test_removeSingleBlock2 :: Assertion
test_removeEmptyStats = assertJustEq (Block 0 [Break, Break]) $
    apply removeEmptyStats $ Block 0 [Break, Empty, Empty, Break, Empty]

test_removeEmptyBlock = assertJustEq Empty $ apply removeEmptyBlock $ Block 0 []
test_removeEmptyBlock2 = assertNothing $ apply removeEmptyBlock $ Block 0 [Break]

test_removeSingleBlock = assertJustEq Break $ apply removeSingleBlock $ Block 0 [Break]
test_removeSingleBlock2 = assertNothing $ apply removeSingleBlock $ Block 0 [Break, Break]

-- If-else statements --------------------------------------

test_reverseNegIfElse = assertJustEq (parseStat "if (x) f(); else t();") $ 
    apply reverseNegIfElse (parseStat "if (!x) t(); else f();")
test_reverseNegIfElse2 = assertNothing $ 
    apply reverseNegIfElse (parseStat "if (x) t(); else f();")

test_removeRedundantIfElse = assertJustEq (parseStat "t();") $ 
    apply removeRedundantIfElse (parseStat "if (x) t(); else t();")
test_removeRedundantIfElse2 = assertNothing $ apply removeRedundantIfElse (parseStat "if (x) t(); else f();")

test_removeEmptyElse = assertJustEq (parseStat "if (x);") $ 
    apply removeEmptyElse (parseStat "if (x) ; else ;")
test_removeEmptyElse2 = assertNothing $
    apply removeEmptyElse (parseStat "if (x) ; else f();")

test_removeEmptyIf = assertJustEq (parseStat "if (!x) f();") $ 
    apply removeEmptyIf (parseStat "if (x) ; else f();")
test_removeEmptyIf2 = assertNothing $ apply removeEmptyIf (parseStat "if (x) t(); else f();")

test_removeUselessIf = assertJustEq (parseStat ";") $ 
    apply (removeUselessIf!!0) (parseStat "if (x) ;")
test_removeUselessIf2 = assertNothing $ apply (removeUselessIf!!0) (parseStat "if (x) t();")
test_removeUselessIf3 = assertJustEq (parseStat ";") $ 
    apply (removeUselessIf!!1) (parseStat "if (x) ; else ;")

test_removeUselessCheck = assertJustEq (parseStat "if (p) a(); else b();") $ 
    apply (removeUselessCheck!!0) (parseStat "if (p) a(); else if (!p) b();")
test_removeUselessCheck2 = assertJustEq (parseStat "if (p) a(); else b();") $ 
    apply (removeUselessCheck!!1) (parseStat "if (p) a(); else if (!p) b(); else c();")

test_collapseIf1 = assertJustEq (parseStat "if (x && y) t();") $ 
    apply (collapseIf!!0) (parseStat "if(x) if (y) t();")
test_collapseIf2 = assertJustEq (parseStat "if (x && y) f(); else if (!x) g();") $ 
    apply (collapseIf!!1) 
    $   IfElse (IdExpr (makeIdentifier "x")) 
            (If (IdExpr (makeIdentifier "y")) 
                (ExprStat (Call (makeIdentifier "f") []))) 
            (ExprStat (Call (makeIdentifier "g") []))
    -- "if(x) {if (y) f();} else g();"

test_simplifyIf = assertJustEq (parseStat "if (x || y) f();") $ 
    apply (simplifyIfs!!0) (parseStat "if (x) f(); else if (y) f();")
test_simplifyIf2 = assertNothing $ 
    apply (simplifyIfs!!0) (parseStat "if (x) f(); else if (y) g();")
test_simplifyIf3 = assertJustEq (parseStat "if (x || y) {f();g();}") $ 
    apply (simplifyIfs!!0) (parseStat "if (x) {f();g();} else if (y) {f();g();}")
test_simplifyIf4 = assertJustEq (parseStat "if (!p && q) b(); else a();") $ 
    apply (simplifyIfs!!1) (parseStat "if (p) a(); else if (q) b(); else a();")
test_simplifyIf5 = assertJustEq (parseStat "if (p || q) a(); else b();") $ 
    apply (simplifyIfs!!2) (parseStat "if (p) a(); else if (q) a(); else b();")

test_ruleExtractFromIf = assertJustEq (forceParseStat "if (p) {a();} else {b();} c();") $ 
    apply extractFromIf (forceParseStat "if (p) {a();c();} else {b();c();} ")

test_ruleRemoveAssignToSelf1 = assertJustEq (forceParseStat ";") $ 
    apply removeAssignToSelf $ forceParseStat "x = x;"
test_ruleRemoveAssignToSelf2 = assertNothing $ apply removeAssignToSelf $ forceParseStat "x = y;"

-- Returns --------------------------------------

-- TODO fix
--test_removeIfElseReturnCond = assertJustEq (forceParseStat "return p || q;") $
--    apply (removeIfElseReturnCond!!0) (forceParseStat "if (p || q) return true; else return false;") 

--test_removeIfElseReturnCond2 = assertJustEq (forceParseStat "return !p;") $
--    apply (removeIfElseReturnCond!!1) (forceParseStat "if (p) return false; else return true;") 

test_removeIfReturnCond = assertJustEq (parseF "return p;") $
    apply (removeIfReturnCond) (parseF "if (p) return true; return false;") 

test_removeIfReturnCond2 = assertJustEq (parseF "return !p;") $
    apply (removeIfReturnCond) (parseF "if (p) return false; return true;") 

test_removeBoolReturnVar = assertJustEq (parseF "return p;") $
    apply removeBoolReturnVar (parseF "if (p) b = true; else b = false; return b;") 

test_removeBoolReturnVar2 = assertJustEq (parseF "return !p;") $
    apply removeBoolReturnVar (parseF "if (p) b = false; else b = true; return b;") 

parseF = programToFragment . forceParseFragment
-- Loop statements --------------------------------------

xsInit = [(Identifier "xs", IntType)]

-- for -> foreach
test_ruleForToForEachBase = validForToForeach xsInit
        "for(int i = 0;i<xs.length;i++) f (xs[i]);"
        "for(int tmp_arrIt : xs) f(tmp_arrIt);"
        
test_ruleForToForEach2 = validForToForeach xsInit
        "for(int i = 0;i<xs.length;i++) if (xs.length == 8) f (xs[i]);"
        "for(int tmp_arrIt : xs) if (xs.length == 8) f(tmp_arrIt);"

test_ruleForToForEach3 = validForToForeach xsInit
        "for(i = 0;i < xs.length;i++) {int x = xs[i]; a+=x; }"
        "for(int tmp_arrIt : xs) {int x = tmp_arrIt; a+=x;} " 

test_ruleForToForEachBaseNoInit = validForToForeach xsInit
        "for(i = 0;i < xs.length;i++) ;"
        "for(int tmp_arrIt : xs) ;" 

test_ruleForToForEachStringType = validForToForeach [(Identifier "xs", StringType)]
        "for(i = 0;i < xs.length;i++) s += xs[i];"
        "for(String tmp_arrIt : xs) s += tmp_arrIt;" 

test_ruleForToForEachBaseAlt = validForToForeach xsInit
        "for(int i = 0;xs.length > i;i=1+i) f(xs[i]);" 
        "for(int tmp_arrIt : xs) f(tmp_arrIt);"

test_ruleForToForEachNotZeroToN = invalidForToForeach "for(i = 1;i <= xs.length;i++) f(xs[i-1]);" xsInit

test_ruleForToForEachIndexUpdated = invalidForToForeach "for(i = 0;i < xs.length;i++) i++;" xsInit

test_ruleForToForEachIndexUsed    = invalidForToForeach "for(i = 0;i < xs.length;i++) f(xs[i+1]);" xsInit

test_ruleForToForEachIndexUsed2 = invalidForToForeach "for(i = 0;i < xs.length;i++) { t = i; f(t); }" xsInit

test_ruleForToForEachIndexUsed3   = invalidForToForeach "for(i = 0;i < xs.length;i++) { f(xs[i]); f(i); }" xsInit

test_ruleForToForEachArrayModified = invalidForToForeach "for(i = 0;i < xs.length;i++) { xs[i] = 1; }" xsInit

test_ruleForToForEachArrayModified2 = invalidForToForeach "for(i = 0;i < xs.length;i++) { t = i; xs[t] = 1; }" xsInit

test_ruleForToForEachWrongIncr = invalidForToForeach "for(i = 0;i < xs.length;i=j+1) ;" xsInit

test_ruleForToForEachBreak = invalidForToForeach "for(i = 0;i < xs.length;i++) if(1) break; " xsInit

-- wordt nog niet geparsed
test_ruleForToForEachArrayModified3 = invalidForToForeach "for(i = 0;i < xs.length;i++) { xs = new int [3]; }" xsInit

-- forToForeach :: ([(Identifier, DataType)], Statement) -> Maybe Statement
invalidForToForeach s dtList = assertNothing $ forToForeach (dtList, forceParseStat s)

validForToForeach dtList from to = assertJustEq (forceParseStat to) (forToForeach (dtList, forceParseStat from))

-- foreach -> for

test_ruleForeachToFor = assertJustEq 
    (forceParseStat "for(int c_i = 0;c_i<a.length;c_i++) { int i = a[c_i]; f(i);}") $ 
    apply foreachToForRule (forceParseStat "for(int i : a) f(i);")

test_ruleForeachToForWithUse = assertJustEq 
    (forceParseStat "for(int c_i = 0;c_i<a.length;c_i++) { int i = a[c_i]; f(i); i = 2; g(i);} ")
    $ apply foreachToForRule (forceParseStat "for(int i : a) { f(i); i = 2; g(i);}")

test_ruleForeachToForString = assertJustEq 
    (forceParseStat "for(int c_s = 0;c_s<xs.length;c_s++) { String s = xs[c_s]; s = \"\";f(s);} ")
    $ apply foreachToForRule (forceParseStat "for(String s : xs) { s = \"\"; f(s); }")

test_ruleForeachToForEx = do
    assertJustEq 
      (forceParseStat "{int[] tmp_getList = getList();for(int c_i = 0;c_i<tmp_getList.length;c_i++) { int i = tmp_getList[c_i]; f(i);}}")
      (apply foreachToForRule (forceParseStat "for(int i: getList()) f(i);"))
    assertJustEq 
      (forceParseStat "{int[] tmp_ab = a.b;for(int c_i = 0;c_i<tmp_ab.length;c_i++) { int i = tmp_ab[c_i]; f(i);}}")
      (apply foreachToForRule (forceParseStat "for(int i: a.b) f(i);"))
    assertJustEq 
      (forceParseStat "{int[] tmp_a = a[0];for(int c_i = 0;c_i<tmp_a.length;c_i++) { int i = tmp_a[c_i]; f(i);}}")
      (apply foreachToForRule (forceParseStat "for(int i: a[0]) f(i);"))

-- Not supported yet
test_ruleForeachToForCollection = assertJustEq 
    (forceParseStat 
        "ArrayList<String> ss = new ArrayList<>(); for (int c_s = 0;c_s<xs.length;c_s++) {String s = ss.get(i);f(s);}")
    $ apply foreachToForRule (forceParseStat "ArrayList<String> ss = new ArrayList<>(); for (String s: ss) { f(s);}")

-- Unneeded conditions in if, e.g. if(x==0||p) y+=x;
test_unneededIfConditions :: Assertion
test_unneededIfConditions = do
    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(x==0 || p) y=y+x;"
        "if (p){ y = y + x;}"
    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(p|| x==0) y=y+x;"
        "if (p){ y=y+x;}"

    -- 3 conditions
    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(p || q || x==0) y=y+x;"
        "if (p || q){ y=y+x;}"
    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(x==0 || p || q) y=y+x;"
        "if (p || q){ y=y+x;}"
    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(p || x==0 || q) y=y+x;"
        "if (p || q){ y=y+x;}"           

    -- ifelse
    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(x==0 || p) y=y+x; else f();"
        "if(p){ y=y+x;} else {f();}"
    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(p || x==0) y=y+x; else f();"
        "if(p){ y=y+x;} else {f();}"

    -- 3 conditions + else
    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(p || q || x==0) y=y+x; else g();"
        "if (p || q){ y=y+x;}else {g();}"
    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(x==0 || p || q) y=y+x; else h();"
        "if (p || q){ y=y+x;}else {h();}"
    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(p || x==0 || q) y=y+x; else i();"
        "if (p || q){ y=y+x;}else {i();}" 

    -- nested
    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(q) if(p||x==0) y+=x;"
        "if(q) if(p) y+=x;" 

test_unneededIfConditionsOtherTypes :: Assertion
test_unneededIfConditionsOtherTypes = do
    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(a || x==0) y=y-x;"
        "if (a){ y=y-x;}"
    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(b || x==1) y=y*x;"
        "if (b){ y=y*x;}"
    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(c|| x==5) y=y+x-5;"
        "if (c){ y=y+x-5;}"

    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(s||x==\"\") y=y+x;"
        "if(s) y=y+x;"

    assertEqBFInCtxt tryRemoveUnneededIfConditionsS
        "if(p|| x==true) y=y && x;"
        "if(p) y=y && x;"
