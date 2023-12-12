{-# LANGUAGE OverloadedStrings #-}

-- Module that exports all necessary Models for the HaQu application
module Haqu.Models where

-- Type definition for QuestionText that is a String
type QuestionText = String
-- Type definition for Options that is a list of String
type Options = [String]
-- Type definition for Answers that is a list of Answer
type Answers = [Answer]

-- Two QuestionTypes are defined. Either TrueFalse that contains a True/False value as solution
-- Or a SingleChoice that solution is a String. The SingleChoice also contains Options to select
data QuestionType = TrueFalse {text::QuestionText, solution::String}
                  | SingleChoice {text::QuestionText, options::Options, solution::String}
                  deriving Show

-- The Quiz contains a quizId, name and a description. Beside that it also contains a List of questions related to this quiz
data Quiz = MkQuiz {
    qid::String, 
    name::String, 
    desc::String, 
    questions::[QuestionType]
} deriving Show

-- An answer contains a questionIndex and the selected Value
data Answer = MkAnswer{questionid:: String, value:: String} deriving Show
