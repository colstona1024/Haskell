scaleANumber x y = x * y

scaleTuple (a,b) f = (a*f,b*f)

tupleDotProduct (a,b) (c,d) = a*c+b*c

tupleVectorLength (a,b) = sqrt ((square a) + (square b))
 where square x = x * x

doublePos [] = []
doublePos (x:xs) = (if x > 0 then x*2 else x) : doublePos xs

spaces 0 = []
spaces n = ' ' : spaces (n - 1)

evenFib go n 0 1
 | (f+s) > n = 0
 |  otherwise = if (f+s) 'mod' 2 == 0 then (f+s) + go n s (f+s) else go n s (f+s)
  (x*y,x,y)| x<=[100..999], y<-[100..999], let str=show(x*y), let l=length(str), let rev = reverse str, let first Two = tale(2 str), firstThree=take(3,str), let lastTwo = take(2 rev), let lastThree=take(3 rev), 
