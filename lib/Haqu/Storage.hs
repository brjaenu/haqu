{-# LANGUAGE OverloadedStrings #-}

module Haqu.Storage where
import System.Directory
import Data.List
import qualified Data.Text as T
import qualified Haqu.Models as M
import qualified Haqu.Utils as U

readQuiz :: String -> IO (Maybe M.Quiz)
readQuiz quizId = do
    let file = "data/" ++ quizId ++ ".txt"
    exists <- doesFileExist file
    if not exists 
    then do return Nothing 
    else do
        content <- readFile file
        let quiz = if null content then Nothing else Just (parseContentToQuiz (lines content))
        return $! quiz

createPlayerQuiz :: String -> String -> IO()
createPlayerQuiz player pid = do
    exists <- doesFileExist file
    if exists 
    then do return () 
    else do
        writeFile file ""
    where file = "data/" ++ pid ++ "/" ++ player ++".txt"


readQuestion :: String -> Int -> IO (Maybe M.QuestionType)
readQuestion quizId questionId = do
    let file = "data/" ++ quizId ++ ".txt"
    exists <- doesFileExist file
    if not exists 
    then do return Nothing 
    else do
        content <- readFile file
        putStrLn ("DEBUG: " ++ concat (lines content))
        putStrLn ("DEBUG: " ++ show (parseQuestions content))
        let question = if null content then Nothing else Just (parseQuestions content !! questionId)
        return $! question

parseQuestions :: String -> [M.QuestionType]
parseQuestions content = map evaluateQuestionType questions
    where questions = tail (map T.unpack (T.splitOn (T.pack "TYPE:") (T.pack content)))

evaluateQuestionType :: String -> M.QuestionType
evaluateQuestionType s = case head ls of
    "FALSETRUE" -> M.TrueFalse (parseQuestionText ls) (parseQuestionSolution ls)
    "SINGLECHOICE" -> M.SingleChoice (parseQuestionText ls) (parseQuestionOptions ls) (parseQuestionSolution ls)
    _ -> error "Error"
    where ls = lines s

parseQuestionText :: [String] -> String
parseQuestionText ls = intercalate ":" (tail (U.split ':' (head questiontexts)))
    where questiontexts = filter (isPrefixOf "Q:") ls

parseQuestionSolution :: [String] -> String
parseQuestionSolution ls = intercalate ":" (tail (U.split ':' (head solutions)))
    where solutions = filter (isPrefixOf "S:") ls

parseQuestionOptions :: [String] -> [String]
parseQuestionOptions ls = map (\l -> intercalate ":" (tail (U.split ':' l))) options
    where options = filter (isPrefixOf "A:") ls

parseContentToQuiz :: [String] -> M.Quiz
parseContentToQuiz ls = M.MkQuiz (extractQuizName ls) (extractQuizDesc ls) []

extractQuizName :: [String] -> String
extractQuizName ls = extractQuizAttribute (head ls)

extractQuizDesc :: [String] -> String
extractQuizDesc ls = extractQuizAttribute (head (tail ls))

extractQuizAttribute :: String -> String
extractQuizAttribute line = intercalate ":" (tail (U.split ':' line))