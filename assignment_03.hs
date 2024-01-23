-- Test:
-- 1. a=4.0 b=8.0 c=5.0: Discriminant is negative, so the equation has no real root []
-- 2. a=3.0 b=-6.0 c=3.0: Discriminant is 0, so the equation has a real root [1.0]
-- 3. a=2.0 b=5.0 c=2.0: Discriminant is positive, so the equation has two real roots [-0.5, -2.0]

import Debug.Trace 

realValuedRoots :: Double -> Double -> Double -> [Double]
realValuedRoots a b c 
    | (b*b-4*a*c) <0 = trace "Discriminant is negative, so the equation has no real root" []
    | (b*b-4*a*c) ==0 = trace "Discriminant is 0, so the equation has a real root" [-b/(2*a)]
    | (b*b-4*a*c) >0 = trace "Discriminant is positive, so the equation has two real roots" [(-b+sqrt (b*b-4*a*c))/(2*a),(-b-sqrt (b*b-4*a*c))/(2*a)] 
