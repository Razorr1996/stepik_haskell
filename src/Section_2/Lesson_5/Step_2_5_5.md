# Инстансы классов типов для `Maybe`

## `Fuctor`

```haskell
instance  Functor Maybe  where
    fmap _ Nothing       = Nothing
    fmap f (Just a)      = Just (f a)
```

## `Applicative`

```haskell
instance Applicative Maybe where
    pure = Just

    Just f  <*> m       = fmap f m
    Nothing <*> _m      = Nothing

    liftA2 f (Just x) (Just y) = Just (f x y)
    liftA2 _ _ _ = Nothing

    Just _m1 *> m2      = m2
    Nothing  *> _m2     = Nothing
```

## `Alternative`

```haskell
instance Alternative Maybe where
    empty = Nothing

    Nothing <|> r = r
    l       <|> _ = l
```

## `Monad`

```haskell
instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing

    (>>) = (*>)
```

## `MonadPlus`

```haskell
instance MonadPlus Maybe
```

Для `Maybe` берётся дефолтная реализация `MonadPlus` от `Alternative`:

```haskell
class (Alternative m, Monad m) => MonadPlus m where
   mzero :: m a
   mzero = empty

   mplus :: m a -> m a -> m a
   mplus = (<|>)
```

# Дистрибутивность для `Alternative` + `Applicative`

```haskell
--(u <|> v) <*> w       =    u <*> w <|> v <*> w
```

## Доказательство

TODO

# Дистрибутивность для `Monad` + `MonadPlus`

```haskell
--(u `mplus` v) >>= k   =    (u >>= k) `mplus` (v >>= k)
```

## Контрпример

В случае, когда в `u` находится `Just (что-то)`, но результат `u >>= k` будет `Nothing`, закон не выполняется.
Конкретный пример:

```haskell
u = Just (-1)

v = Just 1

k x = if x > 0 then (Just x) else Nothing
```

С левой стороны в скобках путем применения `mplus` к `u` и `v` будет выбран самый левый `Just`, то есть `u = Just (-1)`.
А потом после применения оператора bind (`u >>= k`) получится результат `Nothing`, потому что в контейнере лежало
отрицательное число.

С правой же стороны сначала `mplus` посчитает левую часть `(u >>= k)`, где в результате получится `Nothing`, после этого
оператор `mplus` будет вычислять правую часть `(u >>= k)`, где в результате получится `Just 1`. И именно
результат `Just 1` будет финальным.

Таким образом для стандартных представителей `Monad` и `MonadPlus` типа данных `Maybe` закон дистрибутивности не
выполняется.
