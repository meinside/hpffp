data Mood
  = Blah
  | Woot
  deriving (Show, Eq)
  --deriving (Show)

settleDown x =
  if x == Woot
    then Blah
    else x

type Subject = String

type Verb = String

type Object = String

data Sentence =
  Sentence Subject
           Verb
           Object
  deriving (Eq, Show)
