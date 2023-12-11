{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module Haqu.Web where
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as LT
import Data.List (intersperse)
import Data.Char (isDigit)
import Data.Maybe (isNothing)
import qualified Haqu.Storage as S
import qualified Haqu.Models as M
import qualified Haqu.Utils as U

-- Type HTML deklaration 
-- Im Endeffekt ein normaler String
type Html = String

-- Main Funktion
-- Lässt den Scotty Webserver auf Port 3000 laufen
-- Definiert diverse Mappings für Haqu
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

-- Statisches styles.css welches auf jeder Seite verwendet wird
styles :: ActionM ()
styles = do
    setHeader "Content-Type" "text/css"
    file "static/styles.css"

-- URL: /
-- Liest alle Quizzes welche unter /data vorhanden sind und formatiert diese als HTML
getQuizzes :: ActionM ()
getQuizzes = do
    quizzes <- liftIO S.readQuizzes
    html (LT.pack (createPage [
        e "H1" "HaQu",
        e "DIV"
          (unlines [
            createQuizLinks quizzes
          ])
      ]))

-- URL: /quiz/:quizId/start
-- Lädt ein Quiz mit der über die URL angegbene quizId und formatiert diese als HTML
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
-- Lädt eine Statistik-Tabelle mit den Antworten pro Person
-- Zudem wird angezeigt, ob die Antworten korrekt (grün) oder falsch (rot) sind
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
-- Lädt eine Question mit der über die URL angegbene quizId und questionId und formatiert diese als HTML
-- Zudem wird der PlayerName über die QueryParams ausgelesen damit die URL auf der Form angegeben werden kann
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

saveAnswer :: ActionM ()
saveAnswer = do
    playerName <- queryParam "player"
    quizId <- captureParam "quizId"
    questionIdText <- captureParam "questionId"
    option <- formParam "option"
    let questionId = if all isDigit (LT.unpack questionIdText)
                      then read (LT.unpack questionIdText)
                      else 0
    liftIO (S.createAnswer quizId (show questionId) playerName option)
    question <- liftIO (S.readQuestion quizId (questionId+1))
    if isNothing question
    then do redirect $ LT.pack ("/quiz/" ++ quizId ++"/result")
    else do redirect $ LT.pack ("/quiz/" ++ quizId ++"/" ++ show (questionId+1) ++ "?player=" ++ playerName)

registerPlayer :: ActionM ()
registerPlayer = do
    quizId <- captureParam "quizId" :: ActionM String
    playerName <- formParam "player"
    liftIO (S.createQuizPlayer playerName quizId)
    redirect $ LT.pack ("/quiz/"++quizId++"/0?player=" ++ playerName)

createQuizLinks :: [Maybe M.Quiz] -> Html
createQuizLinks [] = []
createQuizLinks (q:qs) = link (U.unwrapMaybe q) ++ createQuizLinks qs
  where link q1 = e "LI" (boldtext q1 ++ M.desc q1 ++ " " ++ ea "A" [("href",startlink q1)] "start")
        boldtext q2 = e "B" ("[" ++ M.qid q2 ++ "] " ++ M.name q2 ++ ": " )
        startlink q3 = "/quiz/" ++ M.qid q3 ++ "/start"

createRegisterForm :: String -> Html
createRegisterForm quizId = ea "FORM" [
  ("method", "post"),
  ("action","/quiz/"++quizId++"/start")] (e "DIV"
    (unlines [
      ea "LABEL" [("for","player")] "Please enter your name:",
      ea "INPUT" [("type","text"),("name","player")] ""
    ]) ++ ea "BUTTON" [("type","submit")] "Start Quiz")

createQuestionForm :: M.QuestionType -> String -> String -> String -> Html
createQuestionForm qt quizId questionId player = ea "FORM" attrs (questionContent ++ submitButton)
  where
    attrs           = [("method", "post"), ("action", url)]
    url             = "/quiz/" ++ quizId ++ "/" ++ questionId ++ "?player=" ++ player
    questionContent = case qt of
      M.TrueFalse t _       -> e "DIV" (unlines [e "LABEL" t, trueFalseRadios])
      M.SingleChoice t os _ -> e "DIV" (unlines [e "LABEL" t, createRadioButtonGroup os 0])
    trueFalseRadios = unlines [createRadioButton "False" "False", createRadioButton "True" "True"]
    submitButton    = ea "BUTTON" [("type", "submit")] "Submit Answer"

createRadioButtonGroup :: [String] -> Int -> Html
createRadioButtonGroup [] _ = []
createRadioButtonGroup (o:os) i = createRadioButton (show i) o ++ createRadioButtonGroup os (i+1)

createRadioButton :: String -> String -> Html
createRadioButton oid t = e "DIV"
    (unlines [
      ea "INPUT" [("type","radio"),("name","option"),("value", oid)] "",
      ea "LABEL" [("for",oid)] t
    ])

createStatisticTable :: M.Quiz -> [(String, [M.Answer])] -> Html
createStatisticTable quiz answers = e "TABLE"
    (unlines [
      createTableHeader quiz,
      createTableRows quiz answers,
      createSummaryRow quiz answers
    ])

createTableHeader :: M.Quiz -> Html
createTableHeader quiz = e "TR"
    (unlines [
      e "TH" "Player",
      createTableHeadElements (length (M.questions quiz))
    ])

createTableRows :: M.Quiz -> [(String, [M.Answer])] -> Html
createTableRows _ []                              = []
createTableRows quiz ((playerName, answers) : as) = e "TR"
    (unlines [
      e "TD" playerName,
      createColumns quiz answers (length (M.questions quiz)-1)
    ]) ++ createTableRows quiz as

createColumns :: M.Quiz -> [M.Answer] -> Int -> Html
createColumns _ _ (-1)         = []
createColumns _ [] _           = []
createColumns quiz (a:as) qInd = column ++ createColumns quiz as (qInd-1)
  where isSolution             = M.value a == M.solution (M.questions quiz !! invQuestionInd)
        invQuestionInd         = length (M.questions quiz) -1 - qInd
        column                 = if isSolution
                                    then ea "TD" [("class", "correct")] (M.value a)
                                    else ea "TD" [("class", "wrong")] (M.value a)

createTableHeadElements :: Int -> Html
createTableHeadElements 0 = ""
createTableHeadElements l = createTableHeadElements (l-1) ++ e "TH" ("Q" ++ show l)

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

emptySummaryColumns :: Int -> Html
emptySummaryColumns 0 = []
emptySummaryColumns n = e "TD" "0 / 0" ++ emptySummaryColumns (n-1)

createSummaryColumns :: M.Quiz -> [M.Answer] -> Int -> Int -> Html
createSummaryColumns _ [] _ _       = []
createSummaryColumns _ _ (-1) _     = []
createSummaryColumns q as invInd am = createSummaryColumns q as (invInd-1) am ++ e "TD" statText
  where
    currentSolution       = M.solution (M.questions q !! invInd)
    amountcorrect         = show (length (filter onlyCorrectAnswers as))
    statText              = amountcorrect ++" / "++ show am
    onlyCorrectAnswers a  = M.value a == currentSolution && M.questionid a == show invInd

-- Erstellt das <html>-Tag mit der lang=en
createPage :: [Html] -> Html
createPage content = "<!DOCTYPE html>" ++ ea "html" attrs (headerPart ++ bodyPart content)
 where attrs = [("lang", "en")]

-- Erstellt einen Header HTML-Tag worin das UTF-8 encoding und das stylesheet gesetzt werden
headerPart :: Html
headerPart =
  e "head" $
    unlines [
      "<meta charset='utf-8'>",
      "<link rel='stylesheet' href='/styles.css'>"
    ]

-- Erstellt einen Body HTML-Tag mit den angegbenen Kinder
bodyPart :: [Html] -> Html
bodyPart content = e "body" (unlines content)

-- Erstellt einen HTML-Tag ohne Attribute
-- Der TagTyp wird über den Parameter definiert
-- Der zweite Parameter definiert die Kinder Tags
e :: String -> Html -> Html
e tag = ea tag []

-- Erstellt einen HTML-Tag mit Attribute
-- Der TagTyp wird über den ersten Parameter definiert
-- Die HTML-Attribute werden über den zweiten Parameter definiert
-- Der dritte Parameter definiert die Kinder Tags
ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where attrsHtml [] = []
        attrsHtml as = " " : intersperse " " (map attrHtml as)
        attrHtml (key, value) = key ++ "='" ++ value ++ "'"
