import Debug.Trace 

divisor :: Integer -> [Integer]
divisor n = divisorRec n n 
    where
        divisorRec :: Integer->Integer->[Integer]
        divisorRec n 0 = []
        divisorRec n d 
            | mod n d == 0 = d:divisorRec n (d-1)
            | mod n d /= 0 = divisorRec n (d-1)


isPrime :: Integer  -> Bool
isPrime n = if (length  (divisor n)) /= 2
            then trace ("No, the number " ++ show n ++ " is not prime. Its divisors are " ++ show (divisor n)) False
            else trace ("Yes, the number " ++ show n ++ " is prime. Its divisors are " ++ show (divisor n)) True
