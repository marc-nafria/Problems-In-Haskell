
-- implementacio llegint per linea
main :: IO ()
main = do
    linea <- getLine
    let paraules = words linea
    let nom = paraules !! 0
    if nom /= "*" then do
        let imcCalculat = imc (read (paraules !! 1) :: Float) (read (paraules !! 2) :: Float)
        let resultat = imcAvaluarResultat imcCalculat
        putStrLn $ nom ++ ": " ++ resultat
        main
    else 
        return ()


-- funcio de IMC, donats dos floats saber el imc
imc :: Float ->  Float -> Float
imc p a = p / (a*a)

-- funcio de avaluacio, donat l'IMC saber l'string que identifica el resultat
imcAvaluarResultat :: Float -> String
imcAvaluarResultat x
    | x < 18 = "underweight"
    | x < 25 = "normal weight"
    | x < 30 = "overweight"
    | x < 40 = "obese"
    | otherwise = "severely obese"