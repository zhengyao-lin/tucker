module Tucker.Service.Event where

{-

two basic parts of tucker service

1. query
    e.g. looking up for txns/blocks

2. event
    e.g. new blocks, new txns

-}

import Tucker.Enc

-- data EventType
--     = NewBlockEvent
--     | NewTxEvent
--     deriving (Show)

-- data EventRegPayload e =
--     EventRegPayload {
--         ev_type :: EventType,
--         ev_cont :: e
--     }

-- class Event e where
--     eventType :: e -> EventType
