module Queue
    ( empty,
      enqueue,
      dequeue
    ) where

import Prelude(error,Int,(++),fst,snd)

type Queue = ([Int],[Int])

empty :: Queue
empty = ([],[])

enqueue :: Queue -> Int -> Queue
enqueue (l1,l2) a = (l1,a:l2) -- (pop,push)

dequeue :: Queue -> (Int,Queue)
--queue is empty
dequeue ([],[]) = (0,empty)

--popping list is non-empty, pushing can be any 
dequeue (h1:t1,pushing) = (e,q) 
  where q = (t1,pushing)
        e = h1 

--popping is empty, push is non-empty
dequeue ([],h2:t2) = dequeue(newQ,[])
  where newQ = reverse (h2:t2)

reverse :: [a] -> [a]
reverse l = reverse' l []
  where reverse' [] acc = acc
        reverse' (h:t) acc = reverse' t (h:acc)