{-# LANGUAGE OverloadedStrings #-}

-- This module handles the storage and retrieval of quizzes, questions, and answers from the filesystem
module Haqu.Storage where
import System.Directory
import Data.List
import qualified Data.Text as T
import qualified Haqu.Models as M
import qualified Haqu.Utils as U

-- Reads all quizzes as Maybe from files under /data
readQuizzes :: IO [Maybe M.Quiz]
readQuizzes = do
    let path = "data/"
    entries <- listDirectory path
    let files = map U.removeExtension (filter (isSuffixOf ".txt") entries)
    quizzes <- sequence $ map readQuiz files
    return $! quizzes

-- Reads a quiz with the specified quizId from the corresponding file
-- Returns Maybe as the requested quiz may not exist
readQuiz :: String -> IO (Maybe M.Quiz)
readQuiz quizId = do
    let file = "data/" ++ quizId ++ ".txt"
    exists <- doesFileExist file
    if not exists
    then do return Nothing
    else do
        content <- readFile file
        let quiz = if null content
            then Nothing
            else do
                let tmpQuiz = parseContentToQuiz quizId (lines content)
                let questions = parseQuestions content
                Just (M.MkQuiz (M.qid tmpQuiz) (M.name tmpQuiz) (M.desc tmpQuiz) questions)
        return $! quiz

-- Reads a question from the file with the corresponding quizId and questionId
-- Access to questions is guaranteed through the index
-- Returns Maybe as the requested question may not exist
readQuestion :: String -> Int -> IO (Maybe M.QuestionType)
readQuestion quizId questionId = do
    let filepath = "data/" ++ quizId ++ ".txt"
    exists <- doesFileExist filepath
    if not exists
    then do return Nothing
    else do
        content <- readFile filepath
        return $! evaluateQuestion content questionId

-- Creates a new file for the given player and quiz
-- If a file already exists, it is recreated
createQuizPlayer :: String -> String -> IO()
createQuizPlayer playerName pid = do
    exists <- doesFileExist file
    if exists
    then do
        removeFile file
        writeFile file ""
    else do
        writeFile file ""
    where file = "data/" ++ pid ++ "/" ++ playerName ++".txt"

-- Creates an answer with the given quizId, questionId, playerName, and value
-- The value is appended to the corresponding file
createAnswer :: String -> String -> String -> String -> IO()
createAnswer quizId questionId playerName value = do
    exists <- doesFileExist file
    if not exists
    then do return ()
    else do
        appendFile file (questionId ++ ":" ++ value ++ "\n")
    where file = "data/" ++ quizId ++ "/" ++ playerName ++".txt"

-- Retrieves answers for a specific quizId, grouped by player
readAnswersByQuizId :: String -> IO [(String, [M.Answer])]
readAnswersByQuizId quizId = do
    let path = "data/" ++ quizId
    entries <- listDirectory path
    let files = map U.removeExtension (filter (isSuffixOf ".txt") entries)
    answers <- mapM (\p -> fmap (\a -> (p, a)) (readAnswersByPlayer quizId p)) files
    return $! answers

-- Retrieves answers for a specific player in a quiz
readAnswersByPlayer :: String -> String -> IO [M.Answer]
readAnswersByPlayer quizId playerName = do
    exists <- doesFileExist file
    if not exists
    then do return []
    else do
        content <- readFile file
        return $! parseAnswers (lines content)
    where file = "data/" ++ quizId ++ "/" ++ playerName ++".txt"

-- Parses lines into a Answer datatype list
parseAnswers :: [String] -> [M.Answer]
parseAnswers = map (\ l -> M.MkAnswer (head (U.split ':' l)) (extractPairValue l))

-- Checks if the given attributes are valid for a Question
-- Returns Maybe as the specified Question may not exist
evaluateQuestion :: String -> Int -> Maybe M.QuestionType
evaluateQuestion content questionId
    | null content      = Nothing
    | isOutOfIndex      = Nothing
    | otherwise         = Just (questions !! questionId)
    where questions     = parseQuestions content
          isOutOfIndex  = length questions <= questionId

-- Parses the content read from the file and separates all QuestionTypes
parseQuestions :: String -> [M.QuestionType]
parseQuestions content = map parseQuestionType questions
    where questions = tail (map T.unpack (T.splitOn (T.pack "TYPE:") (T.pack content)))

-- Distinguishes between available QuestionTypes (FalseTrue and SingleChoice)
-- Creates the corresponding QuestionTypes with the given content
-- Throws an error if the QuestionType is unknown
parseQuestionType :: String -> M.QuestionType
parseQuestionType s = case head ls of
    "FALSETRUE" -> M.TrueFalse
        (parseQuestionText ls)
        (parseQuestionSolution ls)
    "SINGLECHOICE" -> M.SingleChoice
        (parseQuestionText ls)
        (parseQuestionOptions ls)
        (parseQuestionSolution ls)
    _ -> error "Error"
    where ls = lines s -- Maps each line as a String

-- Parses a QuestionText from the lines of a Question
parseQuestionText :: [String] -> String
parseQuestionText ls = intercalate ":" (tail (U.split ':' (head questiontexts)))
    where questiontexts = filter (isPrefixOf "Q:") ls

-- Parses the Solution from the lines of a Question
parseQuestionSolution :: [String] -> String
parseQuestionSolution ls = intercalate ":" (tail (U.split ':' (head solutions)))
    where solutions = filter (isPrefixOf "S:") ls

-- Parses the Options from the lines of a Question
parseQuestionOptions :: [String] -> [String]
parseQuestionOptions ls = map (\l -> intercalate ":" (tail (U.split ':' l))) options
    where options = filter (isPrefixOf "A:") ls

-- Creates the corresponding Quiz from the given lines of a file
parseContentToQuiz :: String -> [String] -> M.Quiz
parseContentToQuiz quizId ls = M.MkQuiz quizId (extractQuizName ls) (extractQuizDesc ls) []

-- Parses the Quizname from the lines of a file
extractQuizName :: [String] -> String
extractQuizName ls = extractPairValue (head ls)

-- Parses the Description from the lines of a file
extractQuizDesc :: [String] -> String
extractQuizDesc ls = extractPairValue (head (tail ls))

-- Parses the actual value of a Key:Value pair from the given line of a file
extractPairValue :: String -> String
extractPairValue line = intercalate ":" (tail (U.split ':' line))