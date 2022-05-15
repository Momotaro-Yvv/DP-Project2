
x = sqrt 9.0

getHead (x:xs) = x

getHead2 (x:xs) = head (x:xs)

getHead3 xs = head xs

mylast xs = xs !!(length xs -1)

x1 = 1

swap (x,y) = (y,x)

double x = x*2

pp xs = reverse xs == xs

twice f x = f (f x)