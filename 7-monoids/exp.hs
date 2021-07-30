newtype Func a = Func (a -> a)

getFunc :: Func a -> a -> a
getFunc (Func f) = f

instance Semigroup (Func a) where
  (<>) (Func f) (Func g) = Func (f . g)

instance Monoid (Func a) where
  mempty = Func id
  mappend f g = f <> g
