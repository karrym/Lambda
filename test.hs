import Data.Char (ord)

fromString [] = \_ n -> n
fromString (x:xs) = \c n -> c (fromNumber $ ord x) (fromString xs c n)

fromNumber 0 = \_ z -> z
fromNumber n = \s z -> s (fromNumber (n-1) s z)
