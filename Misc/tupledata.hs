newtype TL a = TL (Maybe (a, (TL a))) deriving(Eq,Ord)
instance Show b => Show (TL b) where   
    show a = "TL [" ++ showh a
        where showh (TL Nothing) = "]"
              showh (TL (Just (a,TL Nothing))) = show a ++ "]"
              showh (TL (Just (a,b))) = show a ++ ", " ++ showh b
start :: TL a
start = TL Nothing 
tupleList :: (a,TL a) -> TL a
tupleList = TL . Just 
newtype TupleTree a = TupleTree (Maybe (a, (TupleTree a), (TupleTree a))) deriving(Show,Eq,Ord)
end = Nothing 
le = TL end 
tupleTree = TupleTree . Just 
cons :: a -> TL a -> TL a
cons a b = TL $ Just (a,b)
car :: TL a -> a
car (TL (Just (a,b))) = a 
cdr :: TL a -> TL a
cdr (TL (Just (a,b))) = b 
tmap :: (t -> a) -> TL t -> TL a
tmap f (TL Nothing) = (TL Nothing)
tmap f (TL (Just (a,b))) = tupleList (f a, tmap f b)
tzipWith f ox oy = go ox oy 
    where go end _ = le
          go _ end = le
          go x y = tupleList (f (car x) (car y),go (cdr x) (cdr y))