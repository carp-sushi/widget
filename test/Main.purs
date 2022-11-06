module Test.Main where

import Prelude (Unit, discard, pure, show, ($))

import Data.Validation.Semigroup (invalid)
import Effect (Effect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

-- Modules being tested
import Cmd (CmdWidget, Input, Output, execute, mkCmdWidget)
import Cmd.Domain (Name(..), Color(..), Size(..), Material(..), mkWidget)
import Cmd.Errors (Errors(..), mkError)
import Cmd.Validate
  ( validateColor
  , validateMaterial
  , validateName
  , validateSize
  , validateWidget
  )

-- Test widget
testWidget :: CmdWidget
testWidget =
  mkCmdWidget $ mkWidget (Name "test") White Small Carbon

-- Base input instance
base :: Input
base =
  { rule: []
  , widget: testWidget
  }

-- Base output instance
baseOut :: Output
baseOut =
  { widget: testWidget
  , errors: []
  }

-- Command input.
input :: Input
input =
  { rule: [ { name: "ApplyPaint", value: show Black } ]
  , widget: testWidget
  }

-- Expected command output.
expected :: Output
expected =
  { widget: testWidget { paint = show Black } -- Paint should be changed
  , errors: []
  }

-- Command input with multiple actions
multiInput :: Input
multiInput =
  { rule:
      [ { name: "ApplyPaint", value: show Black }
      , { name: "ApplyName", value: "MultiTest" }
      ]
  , widget: testWidget
  }

-- Expected command output.
multiOutput :: Output
multiOutput =
  { widget: testWidget { name = "MultiTest", paint = show Black }
  , errors: []
  }

-- Invalid command input.
invalidInput :: Input
invalidInput =
  { rule:
      [ { name: "ApplyCoating", value: "Wax" }
      , { name: "", value: "" }
      ]
  , widget:
      { name: ""
      , paint: "teal"
      , size: "xl"
      , core: "wax"
      }
  }

-- Command output with expected error messages
errorOutput :: Output
errorOutput =
  { widget:
      { name: ""
      , paint: "teal"
      , size: "xl"
      , core: "wax"
      }
  , errors:
      [ "Invalid action: ApplyCoating"
      , "Action name cannot be empty"
      , "Name cannot be empty"
      , "Invalid color: teal"
      , "Invalid size: xl"
      , "Invalid material: wax"
      ]
  }

-- Run all tests
main :: Effect Unit
main = do
  runTest do
    cmdTest
    widgetTest
    nameTest
    colorTest
    sizeTest
    materialTest

-- Top-level command execution tests.
cmdTest :: TestSuite
cmdTest = do
  suite "command execution should" do
    test "return the expected output" do
      Assert.equal expected (execute input)
    test "apply all actions passed" do
      Assert.equal multiOutput (execute multiInput)
    test "echo input widget on empty actions" do
      Assert.equal baseOut (execute base)
    test "fail on invalid input" do
      Assert.equal errorOutput (execute invalidInput)

-- Test widget validation
widgetTest :: TestSuite
widgetTest = do
  suite "widget validation should" do
    test "return a valid widget" do
      Assert.equal
        (pure $ mkWidget (Name "Test") Black Small Plastic)
        (validateWidget "Test" "Black" "Small" "Plastic")
    test "fail on invalid argument order" do
      Assert.equal
        ( invalid $ Errors
            [ "Invalid color: Carbon"
            , "Invalid size: Black"
            , "Invalid material: Small"
            ]
        )
        (validateWidget "Test" "Carbon" "Black" "Small")

-- Test name validation
nameTest :: TestSuite
nameTest = do
  suite "name validation should" do
    test "create name from string" do
      Assert.equal (pure $ Name "Test") (validateName "Test")
    test "trim spaces from string" do
      Assert.equal (pure $ Name "Test") (validateName " Test ")
    test "fail on empty string" do
      Assert.equal (invalid $ mkError "Name cannot be empty") (validateName "")

-- Test Color validation
colorTest :: TestSuite
colorTest = do
  suite "color validation should" do
    test "create color from valid strings" do
      Assert.equal (pure White) (validateColor "White")
      Assert.equal (pure Black) (validateColor "Black")
      Assert.equal (pure Red) (validateColor "Red")
      Assert.equal (pure Green) (validateColor "Green")
      Assert.equal (pure Blue) (validateColor "Blue")
      Assert.equal (pure Clear) (validateColor "Clear")
    test "trim spaces from string" do
      Assert.equal (pure Red) (validateColor " Red ")
    test "fail on invalid colors" do
      Assert.equal (invalid $ mkError "Invalid color: Teal") (validateColor "Teal")
    test "fail on empty string" do
      Assert.equal (invalid $ mkError "Color cannot be empty") (validateColor "")

-- Test Size validation
sizeTest :: TestSuite
sizeTest = do
  suite "size validation should" do
    test "create size from valid strings" do
      Assert.equal (pure Small) (validateSize "Small")
      Assert.equal (pure Medium) (validateSize "Medium")
      Assert.equal (pure Large) (validateSize "Large")
    test "trim spaces from string" do
      Assert.equal (pure Medium) (validateSize " Medium ")
    test "fail on invalid sizes" do
      Assert.equal (invalid $ mkError "Invalid size: XL") (validateSize "XL")
    test "fail on empty string" do
      Assert.equal (invalid $ mkError "Size cannot be empty") (validateSize "")

-- Test Material validation
materialTest :: TestSuite
materialTest = do
  suite "material validation should" do
    test "create material from valid strings" do
      Assert.equal (pure Carbon) (validateMaterial "Carbon")
      Assert.equal (pure Plastic) (validateMaterial "Plastic")
      Assert.equal (pure Aluminum) (validateMaterial "Aluminum")
      Assert.equal (pure Steel) (validateMaterial "Steel")
    test "trim spaces from string" do
      Assert.equal (pure Steel) (validateMaterial " Steel ")
    test "fail on invalid materials" do
      Assert.equal (invalid $ mkError "Invalid material: Marble") (validateMaterial "Marble")
    test "fail on empty string" do
      Assert.equal (invalid $ mkError "Material cannot be empty") (validateMaterial "")

