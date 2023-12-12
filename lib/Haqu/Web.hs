{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- This module contains all Scotty/web related functions
-- It also contains all functions to parse the Model data to HTML Text
module Haqu.Web where
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import Data.Char (isDigit)
import Data.Maybe (isNothing)
import qualified Data.Text.Lazy as LT
import qualified Haqu.Storage as S
import qualified Haqu.Models as M
import qualified Haqu.Utils as U

-- Type HTML declaration
-- Essentially a regular String
type Html = String

-- Main Function
-- Runs the Scotty web server on port 3000
-- Defines various mappings for Haqu
main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  get  "/styles.css" styles
  get  "/" getQuizzes
  get "/quiz/:quizId/start" getQuiz
  post "/quiz/:quizId/start" registerPlayer
  get "/quiz/:quizId/result" getResult
  get "/quiz/:quizId/:questionId" getQuestion
  post "/quiz/:quizId/:questionId" saveAnswer

-- Static styles.css used on every page
styles :: ActionM ()
styles = do
    setHeader "Content-Type" "text/css"
    file "static/styles.css"

-- URL: /
-- Reads all quizzes under /data and formats them as HTML
getQuizzes :: ActionM ()
getQuizzes = do
    quizzes <- liftIO S.readQuizzes
    html (LT.pack (createPage [
        e "H1" "HaQu",
        e "UL"
          (unlines [
            createQuizLinks quizzes
          ])
      ]))

-- URL: /quiz/:quizId/start
-- Loads a quiz with the specified quizId from the URL and formats it as HTML
getQuiz :: ActionM ()
getQuiz = do
    quizId <- captureParam "quizId"
    quiz <- liftIO (S.readQuiz quizId)
    let qName = maybe "Quiz does not exist" M.name quiz
    html (LT.pack (createPage [
        e "H1" "HaQu",
        e "H2" ("Starting " ++ qName),
        createRegisterForm quizId
      ]))

-- URL: /quiz/:quizId/result
-- Loads a statistics table with answers per person
-- Also indicates whether the answers are correct (green) or wrong (red)
getResult :: ActionM ()
getResult = do
    quizId <- captureParam "quizId"
    quiz <- liftIO (S.readQuiz quizId)
    answersByPlayer <- liftIO (S.readAnswersByQuizId quizId)
    let qName = maybe "Quiz does not exist" M.name quiz
    let qDesc = maybe "Quiz does not exist" M.desc quiz
    html (LT.pack (createPage [
        e "H1" "HaQu",
        e "H2" ("Results: " ++ qName),
        e "P" qDesc,
        createStatisticTable (U.unwrapMaybe quiz) answersByPlayer
      ]))

-- URL: /quiz/:quizId/:questionId
-- Loads a question with the specified quizId and questionId from the URL and formats it as HTML
-- Also reads the playerName from the query params to construct the URL
getQuestion :: ActionM ()
getQuestion = do
    playerName <- queryParam "player"
    quizId <- captureParam "quizId"
    questionIdText <- captureParam "questionId"
    let questionId = if all isDigit (LT.unpack questionIdText)
                      then read (LT.unpack questionIdText)
                      else 0
    question <- liftIO (S.readQuestion quizId questionId)
    if isNothing question 
    then do redirect "/"
    else do
      let q = U.unwrapMaybe question
      html (LT.pack (createPage [
          e "H1" "HaQu",
          createQuestionForm q quizId (show questionId) playerName
        ]))

-- URL: /quiz/:quizId/:questionId?player
-- Saves an answer from the specified player using query params
-- Reads the answer as a formParam
-- Redirects the user either to the next question
-- or to the /result page if there is no more question
saveAnswer :: ActionM ()
saveAnswer = do
    playerName <- queryParam "player"
    quizId <- captureParam "quizId"
    questionIdText <- captureParam "questionId"
    answer <- formParam "answer"
    let questionId = if all isDigit (LT.unpack questionIdText)
                      then read (LT.unpack questionIdText)
                      else 0
    liftIO (S.createAnswer quizId (show questionId) playerName answer)
    question <- liftIO (S.readQuestion quizId (questionId+1))
    if isNothing question
    then do redirect $ LT.pack ("/quiz/" ++ quizId ++"/result")
    else do 
      let resultURL = "/quiz/" ++ quizId ++"/" ++ show (questionId+1) ++ "?player=" ++ playerName
      redirect $ LT.pack resultURL

-- URL: /quiz/:quizId/start
-- Saves a player specified in formParams
-- Redirects the user to the first question of the quiz
registerPlayer :: ActionM ()
registerPlayer = do
    quizId <- captureParam "quizId" :: ActionM String
    playerName <- formParam "player"
    liftIO (S.createQuizPlayer playerName quizId)
    redirect $ LT.pack ("/quiz/"++quizId++"/0?player=" ++ playerName)

-- Parses all given quizzes into an HTML <li> where each quiz has the format
-- [quizId] Title: Description Link.
createQuizLinks :: [Maybe M.Quiz] -> Html
createQuizLinks [] = []
createQuizLinks (q:qs) = link (U.unwrapMaybe q) ++ createQuizLinks qs
  where link q1 = e "LI" (boldtext q1 ++ M.desc q1 ++ " " ++ aTag q1)
        aTag q1 = ea "A" [("href",startlink q1)] "start"
        boldtext q2 = e "B" ("[" ++ M.qid q2 ++ "] " ++ M.name q2 ++ ": " )
        startlink q3 = "/quiz/" ++ M.qid q3 ++ "/start"

-- Creates a registration form for the given quizId.
createRegisterForm :: String -> Html
createRegisterForm quizId = ea "FORM" [
  ("method", "post"),
  ("action","/quiz/"++quizId++"/start")] (e "DIV"
    (unlines [
      ea "LABEL" [("for","player")] "Please enter your name:",
      ea "INPUT" [("type","text"),("name","player")] ""
    ]) ++ ea "BUTTON" [("type","submit")] "Start Quiz")

-- Creates a form for a question with the given questionType, quizId, questionId, and player.
createQuestionForm :: M.QuestionType -> String -> String -> String -> Html
createQuestionForm qt quizId questionId player = ea "FORM" attrs (questionContent++submitButton)
  where
    attrs           = [("method", "post"), ("action", url)]
    url             = "/quiz/" ++ quizId ++ "/" ++ questionId ++ "?player=" ++ player
    questionContent = case qt of
      M.TrueFalse t _       -> e "DIV" (unlines [e "LABEL" t, trueFalseRadios])
      M.SingleChoice t os _ -> e "DIV" (unlines [e "LABEL" t, createRadioButtonGroup os 0])
    trueFalseRadios = unlines [falseRadio, trueRadio]
    trueRadio       = createRadioButton "True" "True"
    falseRadio      = createRadioButton "False" "False"
    submitButton    = ea "BUTTON" [("type", "submit")] "Submit Answer"

-- Creates a group of radio buttons for the given Options.
createRadioButtonGroup :: [String] -> Int -> Html
createRadioButtonGroup [] _ = []
createRadioButtonGroup (o:os) i = radio ++ createRadioButtonGroup os (i+1)
  where radio = createRadioButton (show i) o

-- Creates a single radio button with the given id and label.
createRadioButton :: String -> String -> Html
createRadioButton oid t = e "DIV"
    (unlines [
      ea "INPUT" [("type","radio"),("name","answer"),("value", oid)] "",
      ea "LABEL" [("for",oid)] t
    ])

-- Creates a table containing statistics for the given quiz and player answers.
createStatisticTable :: M.Quiz -> [(String, [M.Answer])] -> Html
createStatisticTable quiz answers = e "TABLE"
    (unlines [
      createTableHeader quiz,
      createTableRows quiz answers,
      createSummaryRow quiz answers
    ])

-- Creates the header row for the statistics table.
createTableHeader :: M.Quiz -> Html
createTableHeader quiz = e "TR"
    (unlines [
      e "TH" "Player",
      createTableHeadElements (length (M.questions quiz))
    ])

-- Creates elements for the header row of the statistics table.
createTableHeadElements :: Int -> Html
createTableHeadElements 0 = ""
createTableHeadElements l = createTableHeadElements (l-1) ++ e "TH" ("Q" ++ show l)

-- Creates table rows for each player in the statistics table.
createTableRows :: M.Quiz -> [(String, [M.Answer])] -> Html
createTableRows _ []                              = []
createTableRows quiz ((playerName, answers) : as) = e "TR"
    (unlines [
      e "TD" playerName,
      createColumns quiz answers (length (M.questions quiz)-1)
    ]) ++ createTableRows quiz as

-- Creates columns for each question and player in the statistics table.
-- For each column the answer will be displayed and colored 
createColumns :: M.Quiz -> [M.Answer] -> Int -> Html
createColumns _ _ (-1)         = []
createColumns _ [] _           = []
createColumns quiz (a:as) qInd = column ++ createColumns quiz as (qInd-1)
  where isSolution             = M.value a == M.solution (M.questions quiz !! invQuestionInd)
        invQuestionInd         = length (M.questions quiz) -1 - qInd
        column                 = if isSolution
                                    then ea "TD" [("class", "correct")] (M.value a)
                                    else ea "TD" [("class", "wrong")] (M.value a)

-- Creates a summary row with overall statistics for the quiz.
-- If no answers were given a summary row will be displayed 
-- with only 0/0 columns for each question
createSummaryRow :: M.Quiz -> [(String, [M.Answer])] -> Html
createSummaryRow quiz []  = e "TR"
    (unlines [
      e "TD" (e "B" "Statistics"),
      emptySummaryColumns (length (M.questions quiz))
    ])
createSummaryRow quiz as  = e "TR"
    (unlines [
      e "TD" (e "B" "Statistics"),
      summaryColumns
    ])
  where summaryColumns = createSummaryColumns quiz allAnswers questionIndex playerAmount
        allAnswers     = concatMap snd as
        questionIndex  = length (M.questions quiz) -1
        playerAmount   = length as

-- Creates empty summary columns for the summary row.
emptySummaryColumns :: Int -> Html
emptySummaryColumns 0 = []
emptySummaryColumns n = e "TD" "0 / 0" ++ emptySummaryColumns (n-1)

-- Creates summary columns for all available questions in the given quiz.
createSummaryColumns :: M.Quiz -> [M.Answer] -> Int -> Int -> Html
createSummaryColumns _ [] _ _       = []
createSummaryColumns _ _ (-1) _     = []
createSummaryColumns q as invInd am = createSummaryColumns q as (invInd-1) am ++ e "TD" stats
  where
    currentSolution       = M.solution (M.questions q !! invInd)
    amountcorrect         = show (length (filter onlyCorrectAnswers as))
    stats              = amountcorrect ++" / "++ show am
    onlyCorrectAnswers a  = M.value a == currentSolution && M.questionid a == show invInd

-- Creates the <html> tag with the lang=en attribute.
createPage :: [Html] -> Html
createPage content = "<!DOCTYPE html>" ++ ea "html" attrs (headerPart ++ bodyPart content)
 where attrs = [("lang", "en")]

-- Creates the header HTML tag with UTF-8 encoding and the stylesheet styles.css.
headerPart :: Html
headerPart =
  e "head" $
    unlines [
      "<meta charset='utf-8'>",
      "<link rel='stylesheet' href='/styles.css'>"
    ]

-- Creates the body HTML tag with the specified children.
bodyPart :: [Html] -> Html
bodyPart content = e "body" (unlines content)

-- Creates an HTML tag without attributes.
-- The tag type is defined by the parameter.
-- The second parameter defines the child tags.
e :: String -> Html -> Html
e tag = ea tag []

-- Creates an HTML tag with attributes.
-- The tag type is defined by the first parameter.
-- HTML attributes are defined by the second parameter.
-- The third parameter defines the child tags.
ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where attrsHtml [] = []
        attrsHtml as = " " : intersperse " " (map attrHtml as)
        attrHtml (key, value) = key ++ "='" ++ value ++ "'"
