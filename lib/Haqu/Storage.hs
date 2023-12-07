{-# LANGUAGE OverloadedStrings #-}

module Haqu.Storage where
import System.Directory
import Data.List
import qualified Data.Text as T
import qualified Haqu.Models as M
import qualified Haqu.Utils as U

-- Liest alle Quizzes aus als Maybe aus den Dateien unter /data
readQuizzes :: IO [Maybe M.Quiz]
readQuizzes = do
    let path = "data/"
    entries <- listDirectory path
    let files = map U.removeExtension (filter (isSuffixOf ".txt") entries)
    quizzes <- sequence $ map readQuiz files
    return $! quizzes

-- Liest ein Quiz mit der angegebenen quizId aus der entsprechenden Datei
-- Zurückgegeben wird ein Maybe da das gesuchte Quiz nicht vorhanden sein muss
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

-- Liest eine Frage aus dem File mit der entsprechenden quizId und questionId
-- Dabei wird der Zugriff auf die Fragen über den Index gewährleistet
-- Zurückgegeben wird ein Maybe da die gesuchte Frage nicht vorhanden sein muss
readQuestion :: String -> Int -> IO (Maybe M.QuestionType)
readQuestion quizId questionId = do
    let filepath = "data/" ++ quizId ++ ".txt"
    exists <- doesFileExist filepath
    if not exists
    then do return Nothing
    else do
        content <- readFile filepath
        return $! evaluateQuestion content questionId

-- Erstellt eine neue Datei für den mitgegebenen Player und Quiz
-- Falls bereits eine Datei existiert wird diese neu erstellt
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

-- Erstellt eine Anwort mit gegebener quizId questionId playerName und Wert
-- Der Wert wird der entsprechenden Datei angefügt
createAnswer :: String -> String -> String -> String -> IO()
createAnswer quizId questionId playerName value = do
    exists <- doesFileExist file
    if not exists
    then do return ()
    else do
        appendFile file (questionId ++ ":" ++ value ++ "\n")
    where file = "data/" ++ quizId ++ "/" ++ playerName ++".txt"

readAnswersByQuizId :: String -> IO [(String, [M.Answer])]
readAnswersByQuizId quizId = do
    let path = "data/" ++ quizId
    entries <- listDirectory path
    let files = map U.removeExtension (filter (isSuffixOf ".txt") entries)
    answers <- mapM (\p -> fmap (\a -> (p, a)) (readAnswersByPlayer quizId p)) files
    return $! answers
    --return $! map (\p -> (p,readAnswersByPlayer quizId p)) files

readAnswersByPlayer :: String -> String -> IO [M.Answer]
readAnswersByPlayer quizId playerName = do
    exists <- doesFileExist file
    if not exists
    then do return []
    else do
        content <- readFile file
        return $! parseAnswers (lines content)
    where file = "data/" ++ quizId ++ "/" ++ playerName ++".txt"

parseAnswers :: [String] -> [M.Answer]
parseAnswers = map (\ l -> M.MkAnswer (head (U.split ':' l)) (extractPairValue l))

-- Überprüft ob die mitgegebenen Attribute valid sind für eine Question
-- Gibt ein Maybe zurück da die angegebene Question nicht vorhanden sein muss
evaluateQuestion :: String -> Int -> Maybe M.QuestionType
evaluateQuestion content questionId
    | null content      = Nothing
    | isOutOfIndex      = Nothing
    | otherwise         = Just (questions !! questionId)
    where questions     = parseQuestions content
          isOutOfIndex  = length questions <= questionId

-- Parst den aus der Datei gelesene Content und separiert alle QuestionTypes
parseQuestions :: String -> [M.QuestionType]
parseQuestions content = map parseQuestionType questions
    where questions = tail (map T.unpack (T.splitOn (T.pack "TYPE:") (T.pack content)))

-- Unterscheided zwichen den zur verfügung stehenden QuestionTypes (FalseTrue und SingleChoice)
-- Erstellt die entsprechenden QuestionTypes mit den mitgegebenen Content
-- Wirft einen Fehler, falls der QuestionType unbekannt ist
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
    where ls = lines s -- Mapped jede Zeile als String

-- Parst einen QuestionText aus den Lines einer Question
parseQuestionText :: [String] -> String
parseQuestionText ls = intercalate ":" (tail (U.split ':' (head questiontexts)))
    where questiontexts = filter (isPrefixOf "Q:") ls

-- Parst die Solution aus den Lines einer Question
parseQuestionSolution :: [String] -> String
parseQuestionSolution ls = intercalate ":" (tail (U.split ':' (head solutions)))
    where solutions = filter (isPrefixOf "S:") ls

-- Parst die Optionen aus den Lines einer Question
parseQuestionOptions :: [String] -> [String]
parseQuestionOptions ls = map (\l -> intercalate ":" (tail (U.split ':' l))) options
    where options = filter (isPrefixOf "A:") ls

-- Erstellt das entsprechende Quiz aus den mitgegebenen Zeilen einer Datei
parseContentToQuiz :: String -> [String] -> M.Quiz
parseContentToQuiz quizId ls = M.MkQuiz quizId (extractQuizName ls) (extractQuizDesc ls) []

-- Parst den Quizname aus den Lines einer Datei
extractQuizName :: [String] -> String
extractQuizName ls = extractPairValue (head ls)

-- Parst die Beschreibung aus den Lines einer Datei
extractQuizDesc :: [String] -> String
extractQuizDesc ls = extractPairValue (head (tail ls))

-- Parst den effektiven Wert eines Key:Value Paires der mitgegebenen Zeile einer Datei
extractPairValue :: String -> String
extractPairValue line = intercalate ":" (tail (U.split ':' line))