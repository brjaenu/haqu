{-# LANGUAGE OverloadedStrings #-}

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

type Html = String 


main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  get  "/styles.css" styles
  get  "/" homeAction
  get "/quiz/:quizId/start" getQuiz
  post "/quiz/:quizId/register" registerPlayer
  get "/quiz/:quizId/:questionId" getQuestion


styles :: ActionM ()
styles = do
    setHeader "Content-Type" "text/css"
    file "static/styles.css"


homeAction :: ActionM ()
homeAction = do
    liftIO (putStrLn "DEBUG: Home Action Called")
    htmlString $ e "H1" "haqu solution"

getQuiz :: ActionM ()
getQuiz = do
    quizId <- captureParam "quizId"
    liftIO (putStrLn ("DEBUG: getQuiz Action Called with param: "++ quizId))
    quiz <- liftIO (S.readQuiz quizId)
    let qName = maybe "Quiz does not exist" M.name quiz
    html (LT.pack (createPage [
        e "H1" "haqu solution", 
        e "H2" ("Starting " ++ qName),
        createRegisterForm quizId
      ]))

getQuestion :: ActionM ()
getQuestion = do 
    quizId <- captureParam "quizId"
    questionIdText <- captureParam "questionId"
    let questionId = if all isDigit (LT.unpack questionIdText)
                                 then read (LT.unpack questionIdText)
                                 else 0 
    liftIO (putStrLn ("DEBUG: getQuestion["++show questionId++"] on Quiz["++quizId++"]"))
    question <- liftIO (S.readQuestion quizId questionId)
    if isNothing question
      then do redirect "/"
    else do
      let q = U.unwrapMaybe question
      html (LT.pack (createPage [
          e "H1" "haqu solution", 
          createQuestionForm q quizId (show questionId)
        ]))

registerPlayer :: ActionM ()
registerPlayer = do
    quizId <- captureParam "quizId" :: ActionM String
    playerName <- formParam "player"
    liftIO (S.createPlayerQuiz playerName quizId)
    liftIO (putStrLn ("DEBUG: registerPlayer["++playerName++"] on Quiz["++quizId++"]"))
    redirect $ LT.pack ("/quiz/"++quizId++"/0")

createRegisterForm :: String -> Html
createRegisterForm quizId = ea "FORM" [
  ("method", "post"),
  ("action","/quiz/"++quizId++"/register")] (e "DIV" 
    (unlines [ 
      ea "LABEL" [("for","player")] "Please enter your name:",
      ea "INPUT" [("type","text"),("name","player")] ""
    ]) ++ ea "BUTTON" [("type","submit")] "Start Quiz")

createQuestionForm :: M.QuestionType -> String -> String -> Html
createQuestionForm (M.TrueFalse t _) quizId questionId = ea "FORM" [
  ("method", "post"),
  ("action","/quiz/"++quizId++"/" ++ questionId)] (e "DIV" 
    (unlines [ 
      ea "LABEL" [("for","solution")] t,
      createRadioButton "False" "False",
      createRadioButton "True" "True"
    ]) ++ ea "BUTTON" [("type","submit")] "Submit Answer")
createQuestionForm (M.SingleChoice t os _) quizId questionId = ea "FORM" [
  ("method", "post"),
  ("action","/quiz/"++quizId++"/" ++ questionId)] (e "DIV" 
    (unlines [ 
      ea "LABEL" [("for","solution")] t,
      createRadioButtonGroup os 0
    ]) ++ ea "BUTTON" [("type","submit")] "Submit Answer")

createRadioButtonGroup :: [String] -> Int -> Html
createRadioButtonGroup [] _ = []
createRadioButtonGroup (o:os) i = (createRadioButton (show i) o) ++ (createRadioButtonGroup os (i+1))

createRadioButton :: String -> String -> Html
createRadioButton oid t = e "DIV" 
    (unlines [ 
      ea "INPUT" [("type","radio"),("name","option"),("value", oid)] "",
      ea "LABEL" [("for",oid)] t
    ])

createPage :: [Html] -> Html
createPage content = "<!DOCTYPE html>" ++ ea "html" [("lang", "en")] (headerPart ++ bodyPart content)

headerPart :: Html
headerPart = 
  e "head" $
    unlines [
      "<meta charset='utf-8'>",
      "<link rel='stylesheet' href='/styles.css'>"
    ]

bodyPart :: [Html] -> Html
bodyPart content = e "body" (unlines content)

htmlString :: String -> ActionM ()
htmlString = html . LT.pack

-- Html DSL
e :: String -> Html -> Html
e tag = ea tag []


ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where attrsHtml [] = []
        attrsHtml as = " " : intersperse " " (map attrHtml as)
        attrHtml (key, value) = key ++ "='" ++ value ++ "'"
