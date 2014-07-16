{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. NetworkDescription t"

import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO
import Control.Monad (when)

-- | Returns sliding window of an event stream.
-- Returns a stream where each event corresponds to
-- the original one, but the value is a list of
-- current value and previous ones, in reverse order.
-- Size of a sliding window is passed in as a parameter.
slide :: Int -> Event t a -> Event t [a]
slide n e = accumE [] $ take n `o` (:) <$> e
    where (f `o` g) x y = f (g x y)  -- big dot, you know

-- | Return a event that fires up only if the history
-- of events matched the pattern provided. The fired
-- value is the pattern in reverse order.
eventSeq :: Eq a => [a] -> Event t a -> Event t [a]
eventSeq pattern e = filterE (== p) $ slide n e
    where n = length pattern
          p = reverse pattern

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

-- Set up the program logic in terms of events and behaviors.
setupNetwork :: forall t. Frameworks t => 
    EventSource Clicked -> Moment t ()
setupNetwork clicked = do
        eclicked <- fromAddHandler $ addHandler clicked

        -- TODO: Friend -> Building will fire both gotoBuilding and
        -- construct. Aww, this is bad.
        let egotoBuilding = eventSeq [Friend, Building] eclicked
            eattackEnemy  = eventSeq [Friend, Enemy]    eclicked
            egotoFriend   = eventSeq [Friend, Friend]   eclicked
            econstruct    = eventSeq [Building]         eclicked

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
