-- ad-hoc algebraic structures
class T f where
    add :: f -> f -> f
    mul :: f -> f -> f
    eq :: f -> f -> Bool
class T f => T1 f where
    ln_pow :: f -> f -> f
class T f => T2 f where
    pow :: f -> f -> f

-- instances
instance T a => T [a] where
    add x y = zipWith add x y
    mul x y = zipWith mul x y
    eq [] [] = True
    eq (x:xs) (y:ys) = eq x y && eq xs ys
    eq _ _ = False
instance T1 a => T1 [a] where
    ln_pow x y = zipWith ln_pow x y
instance T2 a => T2 [a] where
    pow x y = zipWith pow x y

instance T a => T (Maybe a) where
    add (Just x) (Just y) = Just (add x y)
    add _ _ = Nothing
    mul (Just x) (Just y) = Just (mul x y)
    mul _ _ = Nothing
    eq Nothing Nothing = True
    eq (Just x) (Just y) = eq x y
    eq _ _ = False
instance T1 a => T1 (Maybe a) where
    ln_pow (Just x) (Just y) = Just (ln_pow x y)
    ln_pow _ _ = Nothing
instance T2 a => T2 (Maybe a) where
    pow (Just x) (Just y) = Just (pow x y)
    pow _ _ = Nothing

instance T Bool where
    add x y = x || y
    mul x y = x && y
    eq x y = x == y

instance T Integer where
    add x y = x + y
    mul x y = x * y
    eq x y = x == y
instance T2 Integer where
    pow x y = x ^ y

instance T Double where
    add x y = x + y
    mul x y = x * y
    eq x y = abs (x - y) < 1e-6
instance T1 Double where
    ln_pow x y = x ** log y
instance T2 Double where
    pow x y = x ** y

-- laws
-- commutative and associative laws for addition and multiplication
law1 :: T f => f -> f -> Bool
law1 x y = eq (add x y) (add y x)

law2 :: T f => f -> f -> f -> Bool
law2 x y z = eq (add (add x y) z) (add x (add y z))

law3 :: T f => f -> f -> Bool
law3 x y = eq (mul x y) (mul y x)

law4 :: T f => f -> f -> f -> Bool
law4 x y z = eq (mul (mul x y) z) (mul x (mul y z))

-- distributive laws for multiplication over addition
law5d_l :: T f => f -> f -> f -> Bool
law5d_l x y z = eq (mul (add x y) z) (add (mul x z) (mul y z))

law5d_r :: T f => f -> f -> f -> Bool
law5d_r x y z = eq (mul x (add y z)) (add (mul x y) (mul x z))

law6d_l :: T f => f -> f -> f -> Bool
law6d_l x y z = eq (add (mul x y) z) (mul (add x z) (add y z))

law6d_r :: T f => f -> f -> f -> Bool
law6d_r x y z = eq (add x (mul y z)) (mul (add x y) (add x z))

--idempotent laws for addition and multiplication
law5i :: T f => f -> Bool
law5i x = eq (add x x) x

law6i :: T f => f -> Bool
law6i x = eq (mul x x) x

--absorption laws for addition and multiplication
law5a_l :: T f => f -> f -> Bool
law5a_l x y = eq (add x (mul x y)) x

law5a_r :: T f => f -> f -> Bool
law5a_r x y = eq (add (mul x y) y) y

law6a_l :: T f => f -> f -> Bool
law6a_l x y = eq (mul x (add x y)) x

law6a_r :: T f => f -> f -> Bool
law6a_r x y = eq (mul (add x y) y) y

--The law for Laver Shelf
law5ls_l :: T f => f -> f -> f -> Bool
law5ls_l x y z = eq (add x (add y z)) (add (add x y) (add x z))
law5ls_r :: T f => f -> f -> f -> Bool
law5ls_r x y z = eq (add (add x y) z) (add (add x z) (add y z))

law6ls_l :: T f => f -> f -> f -> Bool
law6ls_l x y z = eq (mul x (mul y z)) (mul (mul x y) (mul x z))
law6ls_r :: T f => f -> f -> f -> Bool
law6ls_r x y z = eq (mul (mul x y) z) (mul (mul x z) (mul y z))

-- exponentiation laws
lawe1_l :: T1 f => f -> f -> f -> Bool
lawe1_l x y z = eq (ln_pow (mul x y) z) (mul (ln_pow x z) (ln_pow y z))
lawe1_r :: T1 f => f -> f -> f -> Bool
lawe1_r x y z = eq (ln_pow x (mul y z)) (mul (ln_pow x y) (ln_pow x z))
lawe2_l :: T2 f => f -> f -> f -> Bool
lawe2_l x y z = eq (pow (mul x y) z) (mul (pow x z) (pow y z))
lawe2_r :: T2 f => f -> f -> f -> Bool
lawe2_r x y z = eq (pow x (add y z)) (mul (pow x y) (pow x z))
