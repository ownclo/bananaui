module ReactiveHelpers where

import Reactive.Banana

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

-- | Given a sequence of events, returns a sequence of
-- finite state machine (FSM) states, given an initial state
-- and a transition function as parameters.
fsmEvents :: s -> (e -> s -> s) -> Event t e -> Event t s
fsmEvents start tFunc e = accumE start $ tFunc <$> e
