{-# LANGUAGE OverloadedStrings #-}

module Haqu.Models where

type QuestionText = String
type Options = [String]
type Answers = [Answer]

data QuestionType = TrueFalse {text::QuestionText, solution::String}
                  | SingleChoice {text::QuestionText, options::Options, correctOption::String}
                  deriving Show

data Quiz = MkQuiz {qid::String, name::String, desc::String, questions::[QuestionType]} deriving Show

data Answer = TrueFalseAnswer Int Bool
            | SingleChoiceAnser Int Int
            deriving Show