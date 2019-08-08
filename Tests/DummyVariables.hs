module Tests.DummyVariables where
import Types

testVar :: Var
testVar = Meta "Var"

testVarX :: Var
testVarX = Meta "VarX"

testVarY :: Var
testVarY = Meta "VarY"

testConcrete :: Var
testConcrete = Concrete "Concrete"

testMeta:: Var
testMeta = Meta "Meta"

testConcreteX :: Var
testConcreteX = Concrete "ConcreteX"

testConcreteY :: Var
testConcreteY = Concrete "ConcreteY"

testMetaX :: Var
testMetaX = Meta "MetaX"

testMetaY :: Var
testMetaY = Meta "MetaY"

testMetaZ :: Var
testMetaZ = Meta "MetaZ"

testBind :: Bind
testBind = B testVar testVar

testBindA :: Bind
testBindA = B testVarX testVarX

testBindB :: Bind
testBindB = B testVarY testVarY

testBindC :: Bind
testBindC = B testVarX testVarY

testBindD :: Bind
testBindD = B testVarY testVarX

testCV :: Bind
testCV = CV "test" testVar testVar

testBinds :: [Bind]
testBinds = []

testGamma :: Problem
testGamma = []

testSol :: Sol
testSol = []