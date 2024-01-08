revcat1 [] = []
revcat1 (x:xs) = (revcat1 xs) ++ [x]

revcat2 xs = revcat2' xs []
    where
        revcat2' [] ys = ys
        revcat2' (x:xs) ys = revcat2' xs (x:ys)
