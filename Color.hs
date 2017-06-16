module Color where

data Color = Cross | Circle | Empty

instance Show Color where
    show Cross = "X"
    show Circle = "O"
    show Empty = "_"

instance Eq Color where
    Cross == Cross = True 
    Empty == Empty = True
    Circle == Circle = True
    Cross == Empty = False
    Cross == Circle = False
    Circle == Empty = False
    Circle == Cross = False    
    Empty == Circle = False
    Empty == Cross = False
