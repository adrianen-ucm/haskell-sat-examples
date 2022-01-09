-- |
-- Module      : PropProblemSmtLib
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Module for defining a propositional problem with some SMT-LIB scripting generation capabilities.
module PropProblemSmtLib
  ( Prop (..),
    (~>),
    PropConstraint,
    (=|),
    PropProblem (..),
    checkSatScript,
  )
where

import Data.List (intersperse, nub)

-- | Propositional Logic model.
data Prop
  = Var String
  | Not Prop
  | And [Prop]
  | Or [Prop]
  | If Prop Prop

-- | Operator alias of an If construction.
(~>) :: Prop -> Prop -> Prop
(~>) = If

-- | Collects all the propositional variable names
-- present in a proposition.
varNames :: Prop -> [String]
varNames = nub . go
  where
    go (Var v) = [v]
    go (Not p) = go p
    go (And ps) = mconcat . map go $ ps
    go (Or ps) = mconcat . map go $ ps
    go (If p1 p2) = go p1 <> go p2

-- | A set of constraints with an informative explanation.
data PropConstraint = PropConstraint
  { explanation :: String,
    props :: [Prop]
  }

-- | A propositional problem with some metadata (i.e. name and description)
-- and a set of its explained constraints.
data PropProblem = PropProblem
  { name :: String,
    description :: String,
    constraints :: [PropConstraint]
  }

-- | Operator alias of an explained set of constraints constuction.
(=|) :: String -> [Prop] -> PropConstraint
(=|) = PropConstraint

-- | Collects all the propositional variable names
-- present in a propositional problem.
problemVars :: PropProblem -> [String]
problemVars =
  varNames
    . And
    . mconcat
    . map props
    . constraints

-- | Same as @'unlines' but avoiding a trailing line break.
unlines' :: [String] -> String
unlines' = mconcat . intersperse "\n"

-- | Comment instruction in SMT-LIB, translating line breaks
-- into multiple comments.
comment :: String -> String
comment = unlines' . map ("; " <>) . lines

-- | Declare a boolean variable in SMT-LIB.
declareVar :: String -> String
declareVar v = "(declare-const " <> v <> " Bool)"

-- | Check for satisfiability in SMT-LIB.
checkSat :: String
checkSat = "(check-sat)"

-- | Get the satisfier model in SMT-LIB.
getModel :: String
getModel = "(get-model)"

-- | Propositional construction in SMT-LIB.
prop :: Prop -> String
prop (Var v) = v
prop (Not p) = "(not " <> prop p <> ")"
prop (And ps) = "(and " <> unwords (prop <$> ps) <> ")"
prop (Or ps) = "(or " <> unwords (prop <$> ps) <> ")"
prop (If p1 p2) = "(=> " <> prop p1 <> " " <> prop p2 <> ")"

-- | Assert a proposition in SMT-LIB.
assert :: Prop -> String
assert p = "(assert " <> prop p <> ")"

-- | Generates an SMT-LIB script for checking satisfiability over
-- a propositional problem, including its metadata as comments.
checkSatScript :: PropProblem -> String
checkSatScript p =
  unlines
    [ comment . name $ p,
      comment . description $ p,
      unlines . map declareVar . problemVars $ p,
      unlines' . map assertGroup . constraints $ p,
      checkSat,
      getModel
    ]
  where
    assertGroup g =
      unlines
        [ comment . explanation $ g,
          unlines' . map assert . props $ g
        ]
