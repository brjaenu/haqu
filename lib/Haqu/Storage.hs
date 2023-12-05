{-# LANGUAGE OverloadedStrings #-}

module Haqu.Storage where
import System.Directory
import Data.List
import qualified Data.Text as T
import qualified Haqu.Models as M
import qualified Haqu.Utils as U

readQuizzes :: IO [Maybe M.Quiz]
readQuizzes = do
    let path = "data/"
    entries <- listDirectory path 
    let files = map U.removeExtension (filter (isSuffixOf ".txt") entries)
    quizzes <- sequence $ map readQuiz files
    return $! quizzes

readQuiz :: String -> IO (Maybe M.Quiz)
readQuiz quizId = do
    let file = "data/" ++ quizId ++ ".txt"
    exists <- doesFileExist file
    if not exists 
    then do return Nothing 
    else do
        content <- readFile file
        let quiz = if null content then Nothing else Just (parseContentToQuiz quizId (lines content))
        return $! quiz

createPlayerQuiz :: String -> String -> IO()
createPlayerQuiz player pid = do
    exists <- doesFileExist file
    if exists 
    then do 
        removeFile file
        writeFile file ""
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
        return $! evaluateQuestion content questionId

createAnswer :: String -> String -> String -> String -> IO()
createAnswer quizId questionId playerName value = do
    exists <- doesFileExist file
    if not exists 
    then do return () 
    else do
        appendFile file (questionId ++ ":" ++ value ++ "\n")
    where file = "data/" ++ quizId ++ "/" ++ playerName ++".txt"

evaluateQuestion :: String -> Int -> Maybe M.QuestionType
evaluateQuestion content questionId 
    | null content                      = Nothing
    | length questions <= questionId  = Nothing
    | otherwise                         = Just (questions !! questionId)
    where questions = parseQuestions content

parseQuestions :: String -> [M.QuestionType]
parseQuestions content = map parseQuestionType questions
    where questions = tail (map T.unpack (T.splitOn (T.pack "TYPE:") (T.pack content)))

parseQuestionType :: String -> M.QuestionType
parseQuestionType s = case head ls of
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

parseContentToQuiz :: String -> [String] -> M.Quiz
parseContentToQuiz quizId ls = M.MkQuiz quizId (extractQuizName ls) (extractQuizDesc ls) []

extractQuizName :: [String] -> String
extractQuizName ls = extractQuizAttribute (head ls)

extractQuizDesc :: [String] -> String
extractQuizDesc ls = extractQuizAttribute (head (tail ls))

extractQuizAttribute :: String -> String
extractQuizAttribute line = intercalate ":" (tail (U.split ':' line))