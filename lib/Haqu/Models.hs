{-# LANGUAGE OverloadedStrings #-}

module Haqu.Models where

type QuestionText = String
type Options = [String]
type Answers = [Answer]

data QuestionType = TrueFalse {text::QuestionText, solution::String}
                  | SingleChoice {text::QuestionText, options::Options, solution::String}
                  deriving Show

data Quiz = MkQuiz {qid::String, name::String, desc::String, questions::[QuestionType]} deriving Show

data Answer = MkAnswer{questionid:: String, value:: String} deriving Show

csc :: Quiz -> [Answer] -> Int -> Int -> String
csc _ [] _ _    = []
csc _ _ (-1) _  = []
csc q as invInd am = csc q as (invInd-1) am ++ statText
  where 
    currentSolution = solution (questions q !! invInd)
    amountcorrect   = show (length (filter (\a -> (value a == currentSolution) && questionid a == show invInd) as))
    statText        = amountcorrect ++ " / " ++ show am ++ " - "