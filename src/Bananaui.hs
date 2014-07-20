{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. NetworkDescription t"

import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO
import Control.Monad (when)

import ReactiveHelpers

-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you
type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd


main :: IO ()
main = do
    displayHelpMessage
    source <- newAddHandler
    network <- compile $ setupNetwork source
    actuate network
    eventLoop source

displayHelpMessage :: IO ()
displayHelpMessage = mapM_ putStrLn [
    "-----------------------------",
    "-   THE REACTIVE  TUI FSM   -",
    "------ WIN A BANANA ---------",
    "",
    "Commands are:",
    " building - click on a building",
    " friend   - click on a friend",
    " enemy    - click on an anemy",
    " quit     - guess what? quit",
    ""]

data Clicked = Building
             | Enemy
             | Friend
             deriving (Eq, Show)

data UIState = EmptyState
             | FriendClicked
             deriving (Eq, Show)

data UIAction = ConstructBuilding
              | GotoBuilding
              | GotoFriend
              | AttackEnemy
              deriving (Eq, Show)

type UITransition = (UIState, Maybe UIAction)

uiTMatrix :: Clicked -> UIState -> UITransition
uiTMatrix Friend   EmptyState = (FriendClicked, Nothing)
uiTMatrix Building EmptyState = (EmptyState, Just ConstructBuilding)
uiTMatrix Enemy    EmptyState = (EmptyState, Nothing)

uiTMatrix Friend   FriendClicked = (EmptyState, Just GotoFriend)
uiTMatrix Building FriendClicked = (EmptyState, Just GotoBuilding)
uiTMatrix Enemy    FriendClicked = (EmptyState, Just AttackEnemy)

uiTFunction :: Clicked -> UITransition -> UITransition
uiTFunction c = uiTMatrix c . fst


-- Set up the program logic in terms of events and behaviors.
setupNetwork :: forall t. Frameworks t => 
    EventSource Clicked -> Moment t ()
setupNetwork clicked = do
        eclicked <- fromAddHandler $ addHandler clicked

        let uiTransitions = fsmEvents (EmptyState, Nothing) uiTFunction eclicked
            egotoBuilding = filterAction GotoBuilding
            eattackEnemy  = filterAction AttackEnemy
            egotoFriend   = filterAction GotoFriend
            econstruct    = filterAction ConstructBuilding

            filterAction a = filterE (isAction a) uiTransitions
            isAction a (_, Just a') | a == a' = True
            isAction _ _ = False

        reactimate $ putStrLn "Going to building"       <$ egotoBuilding
        reactimate $ putStrLn "Going to friend"         <$ egotoFriend
        reactimate $ putStrLn "Attacking an enemy"      <$ eattackEnemy
        reactimate $ putStrLn "Constructing a building" <$ econstruct

-- Read commands and fire corresponding events 
eventLoop :: EventSource Clicked -> IO ()
eventLoop clicked = loop
    where
    loop = do
        putStr "> "
        hFlush stdout
        s <- getLine
        case s of
            "building" -> fire clicked Building
            "friend"   -> fire clicked Friend
            "enemy"    -> fire clicked Enemy
            "quit"     -> return ()
            _      -> putStrLn $ s ++ " - unknown command"
        when (s /= "quit") loop
