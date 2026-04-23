module Queue(Queue, enqueue, dequeue, empty, isEmpty, fromList, toList) where
    data Queue a = Queue [a][a] deriving (Eq)
    instance Show a => Show (Queue a) where
        show (Queue f b) = show (f ++ reverse b)