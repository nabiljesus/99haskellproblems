data NestedList a = Elem a | List [NestedList a]
	deriving(Eq,Show)

a = List [Elem 'a', List [Elem 'b', List [Elem 'c', Elem 'd'], Elem 'e' ]]

flatten :: NestedList i -> [i]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List [Elem a]) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs) 

--I don't get why xs need 'List' to work.
