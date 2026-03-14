-- HSpec tests for Val.hs
-- Execute: runhaskell EvalSpec.hs

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Val
import Eval

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    context "*" $ do
        it "multiplies integers" $ do
            eval "*" [Integer 2, Integer 3] `shouldBe` [Integer 6]
        
        it "multiplies floats" $ do
            eval "*" [Integer 2, Real 3.0] `shouldBe` [Real 6.0]
            eval "*" [Real 3.0, Integer 3] `shouldBe` [Real 9.0]
            eval "*" [Real 4.0, Real 3.0] `shouldBe` [Real 12.0]

        it "errors on too few arguments" $ do   
            evaluate (eval "*" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "*" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "+" $ do
        it "adds integers" $ do
            eval "+" [Integer 2, Integer 3] `shouldBe` [Integer 5]
    
        it "adds floats" $ do
            eval "+" [Integer 2, Real 3.0] `shouldBe` [Real 5.0]
            eval "+" [Real 3.0, Integer 3] `shouldBe` [Real 6.0]
            eval "+" [Real 4.0, Real 3.0] `shouldBe` [Real 7.0]

        it "errors on too few arguments" $ do
            evaluate (eval "+" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "+" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "-" $ do
        it "subtracts integers" $ do
            eval "-" [Integer 2, Integer 3] `shouldBe` [Integer 1]
    
        it "subtracts floats" $ do
            eval "-" [Integer 2, Real 3.0] `shouldBe` [Real 1.0]
            eval "-" [Real 3.0, Integer 3] `shouldBe` [Real 0.0]
            eval "-" [Real 4.0, Real 3.0] `shouldBe` [Real (-1.0)]

        it "errors on too few arguments" $ do
            evaluate (eval "-" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "-" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "/" $ do
        it "divides integers" $ do
            eval "/" [Integer 2, Integer 8] `shouldBe` [Integer 4]
    
        it "divides floats" $ do
            eval "/" [Integer 2, Real 8.0] `shouldBe` [Real 4.0]
            eval "/" [Real 2.0, Integer 8] `shouldBe` [Real 4.0]
            eval "/" [Real 2.0, Real 8.0] `shouldBe` [Real 4.0]

        it "errors on too few arguments" $ do
            evaluate (eval "/" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "/" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "^" $ do
        it "raises integers to a power" $ do
            eval "^" [Integer 3, Integer 2] `shouldBe` [Integer 8]
    
        it "raises floats to a power" $ do
            eval "^" [Integer 3, Real 2.0] `shouldBe` [Real 8.0]
            eval "^" [Real 3.0, Integer 2] `shouldBe` [Real 8.0]
            eval "^" [Real 3.0, Real 2.0] `shouldBe` [Real 8.0]

        it "errors on too few arguments" $ do
            evaluate (eval "^" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "^" [Integer 2]) `shouldThrow` errorCall "Stack underflow"


        -- this does not work, seems to be a HSpec bug
        -- it "errors on non-numeric inputs" $ do
        --    evaluate(eval "*" [Real 3.0, Id "x"]) `shouldThrow` anyException

    context "DUP" $ do
        it "duplicates values" $ do
            eval "DUP" [Integer 2] `shouldBe` [Integer 2, Integer 2]
            eval "DUP" [Real 2.2] `shouldBe` [Real 2.2, Real 2.2]
            eval "DUP" [Id "x"] `shouldBe` [Id "x", Id "x"]

        it "errors on empty stack" $ do
            evaluate (eval "DUP" []) `shouldThrow` errorCall "Stack underflow"


    context "EMIT" $ do
        it "emits values" $ do
            eval "EMIT" [Integer 65] `shouldBe` [Id "A"]
            eval "EMIT" [Integer 66] `shouldBe` [Id "B"]
            eval "EMIT" [Integer 66, Real 2.2] `shouldBe` [Id "B", Real 2.2]
            

        it "errors on empty stack" $ do
            evaluate (eval "EMIT" []) `shouldThrow` errorCall "Stack underflow"

    
    context "CR" $ do
        it "pushes a newline string" $ do
            eval "CR" [] `shouldBe` [Id "\n"]

        it "preserves the rest of the stack" $ do
            eval "CR" [Integer 2] `shouldBe` [Id "\n", Integer 2]

    context "STR" $ do
        it "converts an integer to a string" $ do
            eval "STR" [Integer 42] `shouldBe` [Id "42"]

        it "converts a real to a string" $ do
            eval "STR" [Real 2.5] `shouldBe` [Id "2.5"]

        it "leaves an Id as a string" $ do
            eval "STR" [Id "abc"] `shouldBe` [Id "abc"]

        it "errors on empty stack" $ do
            evaluate (eval "STR" []) `shouldThrow` errorCall "Stack underflow"

    context "CONCAT2" $ do
        it "concatenates two strings" $ do
            eval "CONCAT2" [Id "world", Id "hello "] `shouldBe` [Id "hello world"]

        it "preserves the rest of the stack" $ do
            eval "CONCAT2" [Id "B", Id "A", Integer 7] `shouldBe` [Id "AB", Integer 7]

        it "errors on too few arguments" $ do
            evaluate (eval "CONCAT2" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "CONCAT2" [Id "x"]) `shouldThrow` errorCall "Stack underflow"

    context "CONCAT3" $ do
        it "concatenates three strings" $ do
            eval "CONCAT3" [Id "c", Id "b", Id "a"] `shouldBe` [Id "abc"]

        it "preserves the rest of the stack" $ do
            eval "CONCAT3" [Id "3", Id "2", Id "1", Integer 9] `shouldBe` [Id "123", Integer 9]

        it "errors on too few arguments" $ do
            evaluate (eval "CONCAT3" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "CONCAT3" [Id "x"]) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "CONCAT3" [Id "y", Id "x"]) `shouldThrow` errorCall "Stack underflow"

  describe "evalOut" $ do
      context "." $ do
        it "prints top of stack" $ do
            evalOut "." ([Id "x"], "") `shouldBe` ([],"x")
            evalOut "." ([Integer 2], "") `shouldBe` ([], "2")
            evalOut "." ([Real 2.2], "") `shouldBe` ([], "2.2")

        it "errors on empty stack" $ do
            evaluate(evalOut "." ([], "")) `shouldThrow` errorCall "Stack underflow"

      it "eval pass-through" $ do
         evalOut "*" ([Real 2.0, Integer 2], "blah") `shouldBe` ([Real 4.0], "blah") 