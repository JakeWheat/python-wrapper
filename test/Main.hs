

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Test.Tasty as H
import qualified Test.Tasty.HUnit as H

import qualified Data.Text as T
import qualified Text.RawString.QQ as R

import qualified PythonWrapper as Py
--import Control.Concurrent.Async (withAsyncBound, wait)
--import Control.Concurrent (rtsSupportsBoundThreads)

--import Control.Monad (void)

import Control.Monad.Except
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Control.Exception (Exception, catch, ErrorCall, throw)

import Data.Text (Text)
import Control.Concurrent (setNumCapabilities)
import GHC.Conc (getNumProcessors)
import qualified Test.Tasty as T

------------------------------------------------------------------------------



main :: IO ()
main = do
    setNumCapabilities =<< getNumProcessors
    pts <- tests
    T.defaultMain pts

------------------------------------------------------------------------------

tests :: IO H.TestTree
tests = do
    Py.initialize
    pure $ H.testGroup "python-wrapper-tests"
        [bootStrapSyntaxFns
        ,evalTests
        ]

data TestDesc
    = SplitStatementsTest T.Text T.Text (Either Py.PythonError (T.Text, Maybe T.Text))
    | Direct H.TestTree

makeTest :: TestDesc -> H.TestTree
makeTest (SplitStatementsTest nm src ex) = H.testCase (T.unpack nm) $ Py.useBoundThreadIf $ do
    res <- Py._xsplitStatements src
    case (ex, res) of
        (Left (Py.PythonError enm m), Left er) -> compareErrors enm m er
        _ -> H.assertEqual "" ex res
makeTest (Direct t) = t
         
bootStrapSyntaxFns :: H.TestTree
bootStrapSyntaxFns = H.testGroup "bootStrapSyntaxFns" $ map makeTest
    [SplitStatementsTest "split expr" "x + y" (Right ("", Just "x + y"))
    ,SplitStatementsTest "split stmt" "x = y" (Right ("x = y", Nothing))

    ,SplitStatementsTest "split stmt,expr" [R.r|x = y
x + y|] (Right ("x = y", Just "x + y"))
    ,SplitStatementsTest "split stmt,stmt" [R.r|x = y
z = y|] (Right ([R.r|x = y
z = y|], Nothing))

    ,SplitStatementsTest "split empty" "" (Right ("", Nothing))

    ,SplitStatementsTest "split empty" ")this is not correct"
      $ Left $ Py.PythonError "SyntaxError"
        "(\"unmatched ')'\", ('<unknown>', 1, 1, ')this is not correct'"
    ]

-- quick hack because the error formatting changed from python 3.9 to 3.11
compareErrors :: Text -> Text -> Py.PythonError -> IO ()
compareErrors nm pref (Py.PythonError errNm errMsg) = do
    H.assertEqual "" nm errNm
    assertIsPrefixOf pref errMsg

assertIsPrefixOf :: Text -> Text -> IO ()
assertIsPrefixOf a b = do
    let r = a `T.isPrefixOf` b
    if r
        then H.assertBool "" True
        else do
            H.assertFailure $ T.unpack $ a <> "\nisPrefixOf\n" <> b

------------------------------------------------------------------------------
    
evalTests :: H.TestTree
evalTests = H.testGroup "evalTests"
    [testPyScript
    ,testPyScriptRuntimeErrorRet
    ,testPyScriptRuntimeErrorBody
    ,testPyScriptSyntaxError
    ,testPyScript2
    
    ,testPyEval
    ,testPyEvalSyntaxError
    ,testPyEvalRuntimeError
    ,testPyEvalNotExpression
    ,testPyEvalEmpty
    
    ,testPyNone
    
    ,testScriptStmt
    ,testScriptEmpty
    ,testScriptStmtExpr
    ,testScriptFunExpr
    ,testScriptFunExprSep

    ,testPyApp
    ,testPyAppText
    ,testPyAppSyntaxError
    ,testPyAppRuntimeError

    ,testPyGetAttr
    ,testPyGetAttrSyntaxError
    ,testPyGetAttrRuntimeError
    ,testPyApp2
    ,testImportedFun
    
    ,testPyScriptBinds
    ,testStopIteration
    ,testPyForString
    ,testPyForList
    ,testPyForEmptyList
    ,testPyForNoIterMethod
    ,testPyForIterMethodRaises
    ,testPyForIterMethodNotExecutable
    ,testPyForNoNextMethod
    ,testPyForNextMethodRaisesNonStop
    ,testPyForNextMethodNotExecutable
    ,testPyForCallbackRegException
    ,testPyForCallbackHaskellErrorCall
    ,testPyForCallbackHaskellExceptT
    
    -- text, double, int, bool
    ,testTextIn
    ,testTextOut
    ,testTextOutError

    ,testIntIn
    ,testIntOut
    ,testIntOutError

    ,testDoubleIn
    ,testDoubleOut
    ,testDoubleOutError

    ,testBoolIn
    ,testBoolOut
    ,testBoolOutError


    ,testMakePyList
    ,testMakePyTuple
    ,testPyTupleToList
    ,testPyListToList

    -- other data types to consider adding:
    -- datetime, etc.
    -- bytes
    -- complete to and from tuple, list, dict

    ]

takeValue :: Either Py.PythonError a -> IO a
takeValue = either (error . show) pure

takeError :: Show a => Either Py.PythonError a -> IO Py.PythonError
takeError = either pure (error . show)

------------------------------------------------------------------------------

testPyScript :: H.TestTree
testPyScript = H.testCase "testPyScriptSimple" $ Py.useBoundThreadIf $ do
    x <- takeValue =<< Py.script "1 + 2"
    (d :: Int) <- takeValue =<< Py.fromPyObject x
    H.assertEqual "" 3 d

testPyScriptRuntimeErrorRet :: H.TestTree
testPyScriptRuntimeErrorRet = H.testCase "testPyScriptRuntimeErrorRet" $ Py.useBoundThreadIf $ do
    x <- takeError =<< Py.script "1 + sfdsdfa"
    H.assertEqual "" (Py.PythonError "NameError" "name 'sfdsdfa' is not defined") x

testPyScriptRuntimeErrorBody :: H.TestTree
testPyScriptRuntimeErrorBody = H.testCase "testPyScriptRuntimeErrorBody" $ Py.useBoundThreadIf $ do
    x <- takeError =<< Py.script "print(1 + sfdsdfa)\n2"
    H.assertEqual "" (Py.PythonError "NameError" "name 'sfdsdfa' is not defined") x

testPyScriptSyntaxError :: H.TestTree
testPyScriptSyntaxError = H.testCase "testPyScriptSyntaxError" $ Py.useBoundThreadIf $ do
    x <- takeError =<< Py.script ")1 + a"
    compareErrors "SyntaxError" "(\"unmatched ')'\", ('<unknown>', 1, 1, ')1 + a'" x

testPyScript2 :: H.TestTree
testPyScript2 = H.testCase "testPyScript" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script "a = 2"
    x <- takeValue =<< Py.script "a"
    (d :: Int) <- takeValue =<< Py.fromPyObject x
    H.assertEqual "" 2 d

testPyEval :: H.TestTree
testPyEval = H.testCase "testPyEvalSimple" $ Py.useBoundThreadIf $ do
    x <- takeValue =<< Py.eval "1 + 2"
    (d :: Int) <- takeValue =<< Py.fromPyObject x
    H.assertEqual "" 3 d

testPyEvalSyntaxError :: H.TestTree
testPyEvalSyntaxError = H.testCase "testPyEvalSyntaxError" $ Py.useBoundThreadIf $ do
    x <- takeError =<< Py.eval ")1 + 2"
    compareErrors "SyntaxError" "(\"unmatched ')'\", ('', 1, 1, ')1 + 2'" x

testPyEvalRuntimeError :: H.TestTree
testPyEvalRuntimeError = H.testCase "testPyEvalSyntaxError" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script [R.r|
def raiseit():
    raise Exception('stuff')|]
    x <- takeError =<< Py.eval "raiseit()"
    H.assertEqual "" (Py.PythonError "Exception" "stuff") x

testPyEvalNotExpression :: H.TestTree
testPyEvalNotExpression = H.testCase "testPyEvalNotExpression" $ Py.useBoundThreadIf $ do
    x <- takeError =<< Py.eval "1 == 2\n3 == 4"
    compareErrors "SyntaxError" "('invalid syntax', ('', 2, 1, '3 == 4'" x

testPyEvalEmpty :: H.TestTree
testPyEvalEmpty = H.testCase "testPyEvalEmpty" $ Py.useBoundThreadIf $ do
    x <- takeError =<< Py.eval ""
    compareErrors "SyntaxError" "('invalid syntax', ('', 0, 0, '', 0, 0))" x
    --"('unexpected EOF while parsing', ('', 0, 0, ''"

testPyNone :: H.TestTree
testPyNone = H.testCase "testPyNone" $ Py.useBoundThreadIf $ do
    x <- takeValue =<< Py.eval "1"
    t <- Py.isPyNone x
    H.assertBool "" $ not t
    x1 <- takeValue =<< Py.eval "None"
    t1 <- Py.isPyNone x1
    H.assertBool "" t1

testScriptStmt :: H.TestTree
testScriptStmt = H.testCase "testScriptStmt" $ Py.useBoundThreadIf $ do
    x <- takeValue =<< Py.script "x = 3"
    t <- Py.isPyNone x
    H.assertBool "" t

testScriptEmpty :: H.TestTree
testScriptEmpty = H.testCase "testScriptEmpty" $ Py.useBoundThreadIf $ do
    x <- takeValue =<< Py.script ""
    t <- Py.isPyNone x
    H.assertBool "" t

testScriptStmtExpr :: H.TestTree
testScriptStmtExpr = H.testCase "testScriptStmtExpr" $ Py.useBoundThreadIf $ do
    x <- takeValue =<< Py.script "x = 4\nx + 2"
    (d :: Int) <- takeValue =<< Py.fromPyObject x
    H.assertEqual "" 6 d

testScriptFunExpr :: H.TestTree
testScriptFunExpr = H.testCase "testScriptFunExpr" $ Py.useBoundThreadIf $ do
    x <- takeValue =<< Py.script [R.r|
def add(a,b):
    return a + b
add(3,4)|]
    (d :: Int) <- takeValue =<< Py.fromPyObject x
    H.assertEqual "" 7 d

testScriptFunExprSep :: H.TestTree
testScriptFunExprSep = H.testCase "testScriptFunExprSep" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script [R.r|
def add(a,b):
    return a + b|]
    x <- takeValue =<< Py.script "add(3,5)"
    (d :: Int) <- takeValue =<< Py.fromPyObject x
    H.assertEqual "" 8 d

testPyApp :: H.TestTree
testPyApp = H.testCase "testPyAppSimple" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script [R.r|
def add(a,b):
    return a + b|]

    args <- sequence [Py.toPyObject (3 :: Int), Py.toPyObject (33 :: Int)]
    fn <- takeValue =<< Py.eval "add"
    v <- takeValue =<< Py.app fn args
    (d :: Int) <- takeValue =<< Py.fromPyObject v

    H.assertEqual "" 36 d


testPyAppText :: H.TestTree
testPyAppText = H.testCase "testPyAppText" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script [R.r|
def add(a,b):
    return a + b|]
    args <- sequence [Py.toPyObject (3 :: Int), Py.toPyObject (33 :: Int)]
    v <- takeValue =<< Py.appText "add" args
    (d :: Int) <- takeValue =<< Py.fromPyObject v
    H.assertEqual "" 36 d

pyApp2 :: Py.FromPyObject a => Py.PyObject -> [IO Py.PyObject] -> IO a
pyApp2 fn args = takeValue =<< runExceptT (do
    args' <- liftIO $ sequence args
    v <- ExceptT $ Py.app fn args'
    ExceptT $ Py.fromPyObject v)

testPyAppSyntaxError :: H.TestTree
testPyAppSyntaxError = H.testCase "testPyAppSyntaxError" $ Py.useBoundThreadIf $ do
    args <- sequence [Py.toPyObject (3 :: Int), Py.toPyObject (33 :: Int)]
    x <- takeError =<< Py.appText ")add" args
    compareErrors "SyntaxError" "(\"unmatched ')'\", ('', 1, 1, ')add'" x

testPyAppRuntimeError :: H.TestTree
testPyAppRuntimeError = H.testCase "testPyAppRuntimeError" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script [R.r|
def add(a,b):
    return a + b|]

    args <- sequence [Py.toPyObject (3 :: Int), Py.toPyObject (33 :: Int), Py.toPyObject (5 :: Int)]
    x <- takeError =<< Py.appText "add" args
    H.assertEqual "" (Py.PythonError "TypeError" "add() takes 2 positional arguments but 3 were given") x

    fn <- takeValue =<< Py.eval "add"
    
    y <- takeError =<< Py.app fn args
    H.assertEqual "" (Py.PythonError "TypeError" "add() takes 2 positional arguments but 3 were given") y


testPyGetAttr :: H.TestTree
testPyGetAttr = H.testCase "testPyGetAttr" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script [R.r|
class MyClass:
    def __init__(self, a):
        self.a = a
|]
    obj <- takeValue =<< Py.eval "MyClass(10)"
    x <- takeValue =<< ((takeValue =<< Py.getAttr obj "a") >>= Py.fromPyObject)
    H.assertEqual "" (10::Int) x



testPyGetAttrSyntaxError :: H.TestTree
testPyGetAttrSyntaxError = H.testCase "testPyGetAttrSyntaxError" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script [R.r|
class MyClass:
    def __init__(self, a):
        self.a = a
|]
    obj <- takeValue =<< Py.eval "MyClass(10)"
    x <- takeError =<< Py.getAttr obj ") a"
    H.assertEqual "" (Py.PythonError "AttributeError" "'MyClass' object has no attribute ') a'") x

testPyGetAttrRuntimeError :: H.TestTree
testPyGetAttrRuntimeError = H.testCase "testPyGetAttrRuntimeError" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script [R.r|
class MyClass1:
    def __init__(self, a):
        self.a = a
|]
    obj <- takeValue =<< Py.eval "MyClass1(10)"
    x <- takeError =<< Py.getAttr obj "b"
    H.assertEqual "" (Py.PythonError "AttributeError" "'MyClass1' object has no attribute 'b'") x


testPyApp2 :: H.TestTree
testPyApp2 = H.testCase "testPyApp2" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script [R.r|
def add(a,b):
    return a + b
|]

    add <- takeValue =<< Py.eval "add"

    (e :: Int) <- pyApp2 add [Py.toPyObject (3 :: Int),Py.toPyObject (2 :: Int)]
    H.assertEqual "" 5 e

    void $ takeValue =<< Py.script [R.r|
class MyClass:
    def __init__(self, a):
        self.a = a
    def add(self,b):
        return self.a + b
|]

    obj <- takeValue =<< Py.eval "MyClass(10)"
    objAdd <- takeValue =<< Py.getAttr obj "add"
    
    (f :: Int) <- pyApp2 objAdd [Py.toPyObject (3 :: Int)]
    H.assertEqual "" 13 f

testImportedFun :: H.TestTree
testImportedFun = H.testCase "testImportedFun" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script "import math"
    v <- takeValue =<< Py.script "math.factorial(3)"
    (d :: Int) <- takeValue =<< Py.fromPyObject v
    H.assertEqual "" 6 d

    v1 <- takeValue =<< Py.eval "math.factorial(3)"
    (d1 :: Int) <- takeValue =<< Py.fromPyObject v1
    H.assertEqual "" 6 d1
    (e :: Int) <- takeValue =<< ((takeValue =<< (Py.appText "math.factorial" =<< sequence [Py.toPyObject (3 :: Int)]))
                  >>= Py.fromPyObject)
    H.assertEqual "" 6 e


testPyScriptBinds :: H.TestTree
testPyScriptBinds = H.testCase "testPyScriptBinds" $ Py.useBoundThreadIf $ do

    void $ takeValue =<< Py.script [R.r|
def f(x):
    return x + 1
                  |]

    myInt <- Py.toPyObject (4 :: Int)
    v1 <- takeValue =<< Py.scriptWithBinds [("aa", myInt)] "f(aa)"
    v <- takeValue =<< (Py.fromPyObject v1)
    
    H.assertEqual "" (5 :: Int) v


testStopIteration :: H.TestTree
testStopIteration = H.testCase "testStopIteration" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script "x = []\ni = x.__iter__()\n"
    --void $ takeValue =<< Py.script "i.__next__()"
    it <- takeError =<< Py.eval "i.__next__()"
    --putStrLn "y3"
    H.assertEqual "" (Py.PythonError "StopIteration" "") it
    --print it 
    --putStrLn "y4"
    --undefined

testPyForString :: H.TestTree
testPyForString = H.testCase "testPyForString" $ Py.useBoundThreadIf $ do
    b <- Py.toPyObject ("banana" :: T.Text)
    r <- takeValue =<< (Py.for b $ \c -> do
        u <- takeValue =<< Py.getAttr c "upper"
        x <- takeValue =<< Py.app u []
        pure x
                       )
    rf <- takeValue =<< Py.eval("''.join")
    args <- takeValue =<< Py.makePyList r
    x <- takeValue =<< Py.app rf [args]
    r' <- takeValue =<< Py.fromPyObject x
    H.assertEqual "" ("BANANA" :: T.Text) r'

testPyForList :: H.TestTree
testPyForList = H.testCase "testPyForList" $ Py.useBoundThreadIf $ do
    b <- takeValue =<< Py.eval "[1,2,3]"
    pnms <- takeValue =<< (Py.for b $ \c -> do
        (n :: Int) <- takeValue =<< Py.fromPyObject c
        Py.toPyObject (n * 2))
    x3 :: [Int] <- either (error . show) id . sequence <$> mapM Py.fromPyObject pnms
    H.assertEqual "" [2,4,6] x3

testPyForEmptyList :: H.TestTree
testPyForEmptyList = H.testCase "testPyForEmptyList" $ Py.useBoundThreadIf $ do
    b <- takeValue =<< Py.eval "[]"
    pnms <- takeValue =<< Py.for b pure
    x3 :: [Int] <- either (error . show) id . sequence <$> mapM Py.fromPyObject pnms
    H.assertEqual "" [] x3


{-

working iterator example:

class Count:

    def __init__(self, end=0):
        self.num = 0
        self.end = end

    def __iter__(self):
        return self

    def __next__(self):
        if self.num > self.end:
            raise StopIteration
        num = self.num
        self.num += 1
        return num

for i in Count(5):
    print(i)


output:
0
1
2
3
4
5

-}

testPyForNoIterMethod :: H.TestTree
testPyForNoIterMethod = H.testCase "testPyForNoIterMethod" $ Py.useBoundThreadIf $ do
    b <- takeValue =<< Py.eval "123"
    e <- takeError =<< Py.for b pure
    H.assertEqual "" (Py.PythonError "AttributeError" "'int' object has no attribute '__iter__'") e

testPyForIterMethodRaises :: H.TestTree
testPyForIterMethodRaises = H.testCase "testPyForIterMethodRaises" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script [R.r|
class Count1:
    def __init__(self, end=0):
        self.num = 0
        self.end = end
    def __iter__(self):
        raise Exception("denied1")
        return self
    def __next__(self):
        if self.num > self.end:
            raise StopIteration
        num = self.num
        self.num += 1
        return num
    |]
    i <- takeValue =<< Py.eval "Count1(5)"
    e <- takeError =<< Py.for i pure
    H.assertEqual "" (Py.PythonError "Exception" "denied1") e

testPyForIterMethodNotExecutable :: H.TestTree
testPyForIterMethodNotExecutable = H.testCase "testPyForIterMethodNotExecutable" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script [R.r|
class Count2:
    def __init__(self, end=0):
        self.num = 0
        self.end = end
    def __iter__(self):
        return "124"
    def __next__(self):
        if self.num > self.end:
            raise StopIteration
        num = self.num
        self.num += 1
        return num
    |]
    i <- takeValue =<< Py.eval "Count2(5)"
    e <- takeError =<< Py.for i pure
    H.assertEqual "" (Py.PythonError "AttributeError" "'str' object has no attribute '__next__'") e

testPyForNoNextMethod :: H.TestTree
testPyForNoNextMethod = H.testCase "testPyForNoNextMethod" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script [R.r|
class Count3:
    def __init__(self, end=0):
        self.num = 0
        self.end = end
    def __iter__(self):
        return self
    def not__next__(self):
        if self.num > self.end:
            raise StopIteration
        num = self.num
        self.num += 1
        return num
    |]
    i <- takeValue =<< Py.eval "Count3(5)"
    e <- takeError =<< Py.for i pure
    H.assertEqual "" (Py.PythonError "AttributeError" "'Count3' object has no attribute '__next__'") e

testPyForNextMethodRaisesNonStop :: H.TestTree
testPyForNextMethodRaisesNonStop = H.testCase "testPyForNextMethodRaisesNonStop" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script [R.r|
class Count4:
    def __init__(self, end=0):
        self.num = 0
        self.end = end
    def __iter__(self):
        return self
    def __next__(self):
        raise Exception("denied2")
        if self.num > self.end:
            raise StopIteration
        num = self.num
        self.num += 1
        return num
    |]
    i <- takeValue =<< Py.eval "Count4(5)"
    e <- takeError =<< Py.for i pure
    H.assertEqual "" (Py.PythonError "Exception" "denied2") e

testPyForNextMethodNotExecutable :: H.TestTree
testPyForNextMethodNotExecutable = H.testCase "testPyForNextMethodNotExecutable" $ Py.useBoundThreadIf $ do
    void $ takeValue =<< Py.script [R.r|
class Count5:
    def __init__(self, end=0):
        self.num = 0
        self.end = end
        self.__next__ = True
    def __iter__(self):
        return self
    def not__next__(self):
        if self.num > self.end:
            raise StopIteration
        num = self.num
        self.num += 1
        return num
    |]
    i <- takeValue =<< Py.eval "Count5(5)"
    e <- takeError =<< Py.for i pure
    H.assertEqual "" (Py.PythonError "TypeError" "'bool' object is not callable") e


data MyException = ThisException
    deriving (Eq,Show)

instance Exception MyException

testPyForCallbackRegException :: H.TestTree
testPyForCallbackRegException = H.testCase "testPyForCallbackRegException" $ Py.useBoundThreadIf $ do
    b <- takeValue =<< Py.eval "[1]"
    r <- catch (do
                void $ takeValue =<< Py.for b (throw ThisException)
                pure False
               )
             (\(_ :: MyException) -> pure True)
    H.assertEqual "" r True


testPyForCallbackHaskellErrorCall :: H.TestTree
testPyForCallbackHaskellErrorCall = H.testCase "testPyForCallbackHaskellErrorCall" $ Py.useBoundThreadIf $ do
    b <- takeValue =<< Py.eval "[1]"
    r <- catch (do
                void $ takeValue =<< Py.for b (error "hello")
                pure False
               )
             (\(_ :: ErrorCall) -> pure True)
    H.assertEqual "" r True

testPyForCallbackHaskellExceptT :: H.TestTree
testPyForCallbackHaskellExceptT = H.testCase "testPyForCallbackHaskellExceptT" $ Py.useBoundThreadIf $ do
    b <- takeValue =<< Py.eval "[1]"
    (r :: Either MyException ()) <- runExceptT (do
                _ <- either (error . show) id <$> Py.for b (const $ throwError ThisException)
                pure ())
    H.assertEqual "" r (Left ThisException)

testTextIn :: H.TestTree
testTextIn = H.testCase "testTextIn" $ Py.useBoundThreadIf $ do
    let a :: T.Text = "asdf"
    a' <- Py.toPyObject a
    v <- takeValue =<< Py.scriptWithBinds [("aa", a')] "str(type(aa))"
    (v1 :: T.Text) <- takeValue =<< Py.fromPyObject v
    H.assertEqual "" v1 "<class 'str'>"

testTextOut :: H.TestTree
testTextOut = H.testCase "testTextOut" $ Py.useBoundThreadIf $ do
    v <- takeValue =<< Py.eval "'sdfg'"
    (v1 :: T.Text) <- takeValue =<< Py.fromPyObject v
    H.assertEqual "" v1 "sdfg"

testTextOutError :: H.TestTree
testTextOutError = H.testCase "testTextOutError" $ Py.useBoundThreadIf $ do
    v <- takeValue =<< Py.eval "1"
    e <- takeError =<< (Py.fromPyObject v :: IO (Either Py.PythonError T.Text))
    H.assertEqual "" (Py.PythonError "TypeMismatch" "Expected str, got int") e

testIntIn :: H.TestTree
testIntIn = H.testCase "testIntIn" $ Py.useBoundThreadIf $ do
    let a :: Int = 2323
    a' <- Py.toPyObject a
    v <- takeValue =<< Py.scriptWithBinds [("aa", a')] "str(type(aa))"
    (v1 :: T.Text) <- takeValue =<< Py.fromPyObject v
    H.assertEqual "" v1 "<class 'int'>"

testIntOut :: H.TestTree
testIntOut = H.testCase "testIntOut" $ Py.useBoundThreadIf $ do
    n <- takeValue =<< Py.eval "2324"
    (n1 :: Int) <- takeValue =<< Py.fromPyObject n
    H.assertEqual "" n1 2324

testIntOutError :: H.TestTree
testIntOutError = H.testCase "testIntOutError" $ Py.useBoundThreadIf $ do
    v <- takeValue =<< Py.eval "True"
    e <- takeError =<< (Py.fromPyObject v :: IO (Either Py.PythonError Int))
    H.assertEqual "" (Py.PythonError "TypeMismatch" "Expected int, got bool") e

    v1 <- takeValue =<< Py.eval "1.0"
    e1 <- takeError =<< (Py.fromPyObject v1 :: IO (Either Py.PythonError Int))
    H.assertEqual "" (Py.PythonError "TypeMismatch" "Expected int, got float") e1



testDoubleIn :: H.TestTree
testDoubleIn = H.testCase "testDoubleIn" $ Py.useBoundThreadIf $ do
    let a :: Double = 2323.1
    a' <- Py.toPyObject a
    v <- takeValue =<< Py.scriptWithBinds [("aa", a')] "str(type(aa))"
    (v1 :: T.Text) <- takeValue =<< Py.fromPyObject v
    H.assertEqual "" v1 "<class 'float'>"

testDoubleOut :: H.TestTree
testDoubleOut = H.testCase "testDoubleOut" $ Py.useBoundThreadIf $ do
    n <- takeValue =<< Py.eval "2324.1"
    (n1 :: Double) <- takeValue =<< Py.fromPyObject n
    H.assertEqual "" n1 2324.1

    m <- takeValue =<< Py.eval "2.0"
    (m1 :: Double) <- takeValue =<< Py.fromPyObject m
    H.assertEqual "" m1 2


testDoubleOutError :: H.TestTree
testDoubleOutError = H.testCase "testDoubleOutError" $ Py.useBoundThreadIf $ do
    v <- takeValue =<< Py.eval "True"
    e <- takeError =<< (Py.fromPyObject v :: IO (Either Py.PythonError Double))
    H.assertEqual "" (Py.PythonError "TypeMismatch" "Expected float, got bool") e

    v1 <- takeValue =<< Py.eval "1"
    e1 <- takeError =<< (Py.fromPyObject v1 :: IO (Either Py.PythonError Double))
    H.assertEqual "" (Py.PythonError "TypeMismatch" "Expected float, got int") e1


testBoolIn :: H.TestTree
testBoolIn = H.testCase "testBoolIn" $ Py.useBoundThreadIf $ do
    let a = True
    a' <- Py.toPyObject a
    v <- takeValue =<< Py.scriptWithBinds [("aa", a')] "str(type(aa))"
    (v1 :: T.Text) <- takeValue =<< Py.fromPyObject v
    H.assertEqual "" v1 "<class 'bool'>"

testBoolOut :: H.TestTree
testBoolOut = H.testCase "testBoolOut" $ Py.useBoundThreadIf $ do
    n <- takeValue =<< Py.eval "True"
    (n1 :: Bool) <- takeValue =<< Py.fromPyObject n
    H.assertEqual "" n1 True

    m <- takeValue =<< Py.eval "False"
    (m1 :: Bool) <- takeValue =<< Py.fromPyObject m
    H.assertEqual "" m1 False

testBoolOutError :: H.TestTree
testBoolOutError = H.testCase "testBoolOutError" $ Py.useBoundThreadIf $ do
    v <- takeValue =<< Py.eval "1"
    e <- takeError =<< (Py.fromPyObject v :: IO (Either Py.PythonError Bool))
    H.assertEqual "" (Py.PythonError "TypeMismatch" "Expected bool, got int") e


testMakePyList :: H.TestTree
testMakePyList = H.testCase "testMakePyList" $ Py.useBoundThreadIf $ do
    mtlst <- takeValue =<< Py.makePyList []
    _x <- takeValue =<< Py.scriptWithBinds [("lst", mtlst)] [R.r|
if lst != []:
    raise Exception("expected [], got " + str(lst))
                                         |]
    v <- takeValue =<< Py.eval "1"
    mtlst1 <- takeValue =<< Py.makePyList [v]
    _x <- takeValue =<< Py.scriptWithBinds [("lst", mtlst1)] [R.r|
if lst != [1]:
    raise Exception("expected [1], got " + str(lst))
                                         |] 
    v2 <- takeValue =<< Py.eval "2"
    mtlst2 <- takeValue =<< Py.makePyList [v,v2]
    _x <- takeValue =<< Py.scriptWithBinds [("lst", mtlst2)] [R.r|
if lst != [1,2]:
    raise Exception("expected [1,2], got " + str(lst))
                                         |] 
    pure ()

        
    
testMakePyTuple :: H.TestTree
testMakePyTuple = H.testCase "testMakePyTuple" $ Py.useBoundThreadIf $ do
    mtlst <- takeValue =<< Py.makePyTuple []
    _x <- takeValue =<< Py.scriptWithBinds [("lst", mtlst)] [R.r|
if lst != ():
    raise Exception("expected (), got " + str(lst))
                                         |]
    v <- takeValue =<< Py.eval "1"
    mtlst1 <- takeValue =<< Py.makePyTuple [v]
    _x <- takeValue =<< Py.scriptWithBinds [("lst", mtlst1)] [R.r|
if lst != (1,):
    raise Exception("expected (1,), got " + str(lst))
                                         |] 
    v2 <- takeValue =<< Py.eval "2"
    mtlst2 <- takeValue =<< Py.makePyTuple [v,v2]
    _x <- takeValue =<< Py.scriptWithBinds [("lst", mtlst2)] [R.r|
if lst != (1,2):
    raise Exception("expected (1,2), got " + str(lst))
                                         |] 
    pure ()

testPyTupleToList :: H.TestTree
testPyTupleToList = H.testCase "testPyTupleToList" $ Py.useBoundThreadIf $ do
    v1 <- takeValue =<< Py.eval "()"
    lst <- takeValue =<< Py.pyTupleToList v1
    H.assertBool "" (null lst)

    v2 <- takeValue =<< Py.eval "(1,)"
    lst1 <- takeValue =<< Py.pyTupleToList v2
    case lst1 of
        [a] -> do
            (a' :: Int) <- takeValue =<< Py.fromPyObject a
            H.assertEqual "" 1 a'
        _ -> H.assertFailure $ "expected one element, got " ++ show (length lst)

    v3 <- takeValue =<< Py.eval "(1,'b')"
    lst2 <- takeValue =<< Py.pyTupleToList v3
    case lst2 of
        [a,b] -> do
            (a' :: Int) <- takeValue =<< Py.fromPyObject a
            H.assertEqual "" 1 a'
            (b' :: T.Text) <- takeValue =<< Py.fromPyObject b
            H.assertEqual "" "b" b'
        _ -> H.assertFailure $ "expected two elements, got " ++ show (length lst)

    v4 <- takeValue =<< Py.eval "['test']"
    e <- takeError =<< Py.pyTupleToList v4
    H.assertEqual "" (Py.PythonError "TypeMismatch" "Expected tuple, got list") e

testPyListToList :: H.TestTree
testPyListToList = H.testCase "testPyListToList" $ Py.useBoundThreadIf $ do
    v1 <- takeValue =<< Py.eval "[]"
    lst <- takeValue =<< Py.pyListToList v1
    H.assertBool "" (null lst)

    v2 <- takeValue =<< Py.eval "[1]"
    lst1 <- takeValue =<< Py.pyListToList v2
    case lst1 of
        [a] -> do
            (a' :: Int) <- takeValue =<< Py.fromPyObject a
            H.assertEqual "" 1 a'
        _ -> H.assertFailure $ "expected one element, got " ++ show (length lst)

    v3 <- takeValue =<< Py.eval "[1,'b']"
    lst2 <- takeValue =<< Py.pyListToList v3
    case lst2 of
        [a,b] -> do
            (a' :: Int) <- takeValue =<< Py.fromPyObject a
            H.assertEqual "" 1 a'
            (b' :: T.Text) <- takeValue =<< Py.fromPyObject b
            H.assertEqual "" "b" b'
        _ -> H.assertFailure $ "expected two elements, got " ++ show (length lst)

    v4 <- takeValue =<< Py.eval "('test',)"
    e <- takeError =<< Py.pyListToList v4
    H.assertEqual "" (Py.PythonError "TypeMismatch" "Expected list, got tuple") e
