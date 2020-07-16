{-# language BlockArguments #-}

module Test.LogicSpec where

import Tree
import Logic
import Test.Tasty.Extensions hiding (eval)
import Prelude hiding ((*), and, or, not)

test_and = test "and" do
  eval (and * true * true) === true
  eval (and * true * false) === false
  eval (and * false * true) === false
  eval (and * false * false) === false

test_or = test "or" do
  eval (or * true * true) === true
  eval (or * true * false) === true
  eval (or * false * true) === true
  eval (or * false * false) === false

test_not = test "not" do
  eval (not * true) === false
  eval (not * false) === true

test_implies = test "implies" do
  eval (implies * true * true) === true
  eval (implies * true * false) === false
  eval (implies * false * true) === true
  eval (implies * false * false) === true

test_iff = test "iff" do
  eval (iff * true * true) === true
  eval (iff * true * false) === false
  eval (iff * false * true) === false
  eval (iff * false * false) === true
