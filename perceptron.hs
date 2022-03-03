sign :: (Num a, Ord a, Show a) => a -> a
sign x | x == 0 = 0
       | x > 0 = 1
       | x < 0 = -1

loss :: (Num a, Ord a, Fractional a, Show a) => a -> a -> a
loss x y = if x == y then 0 else 1

(/+/) :: (Num a, Ord a, Fractional a, Show a) => [a] -> [a] -> [a] -- vector addition
(/+/) = zipWith (+)

(/*/) :: (Num a, Ord a, Fractional a, Show a) => [a] -> a -> [a] -- scalar multiplication
xs /*/ a = map (*a) xs

zeros :: (Num a, Ord a, Fractional a, Show a) => [a] -> [a] -- list of zeros of equal length to the input list
zeros = map (* 0)

linearClassify :: (Num a, Ord a, Show a) => [a] -> [a] -> a -> a
linearClassify x theta theta_0 = sign (dotProdTranspose theta x + theta_0) where
    dotProdTranspose xs ys = sum (zipWith (*) xs ys) -- equivalent to xs.T * ys


predictions :: (Num a, Ord a, Show a) => [[a]] -> [a] -> a -> [a]
predictions data_in theta theta0 = [linearClassify x theta theta0 | x <- data_in]   

avgLoss :: (Num a, Ord a, Fractional a, Show a) => [a] -> [a] -> a
avgLoss guesses labels = sum (zipWith loss guesses labels) / fromIntegral (length guesses)

meanError :: (Num a, Ord a, Fractional a, Show a) => 
    [[a]] -> --data, each sub array is a point
    [a] -> --labels
    ([a], a) -> --theta, theta zero
    a
meanError data_in labels (theta, theta0) = avgLoss preds labels where
    preds = [linearClassify x theta theta0 | x <- data_in]

perceptronStep :: (Num a, Ord a, Fractional a, Show a) => 
    [[a]] -> -- data
    [a] -> -- labels
    [a] -> -- starting theta
    a -> -- starting theta0
    ([a], a) -- output (theta, theta0)

perceptronStep data_in labels theta theta0 = foldr func (theta, theta0) (zip data_in labels) where
    func (x, label) (t, t0) | linearClassify x t t0 * label > 0 = (t, t0)
                            | otherwise = (t /+/ (x /*/ label), t0 + label)


perceptron :: (Num a, Ord a, Fractional a, Show a) => 
    [[a]] -> -- data
    [a] -> -- labels
    a -> -- max iterations
    ([a], a) -> -- starting theta, theta0
    ([a], a) -- output (theta, theta0)
perceptron _ _ 0 (t, t0) = (t, t0)
perceptron xs ls steps (t, t0) = perceptron xs ls (steps - 1) (perceptronStep xs ls t t0)


testPerceptron :: (Num a, Ord a, Fractional a, Show a) => [[a]] -> [a] -> a -> a
testPerceptron data_in labels steps = meanError x y (perceptron data_in labels steps (zeros labels, 0))

thetasToString :: (Num a, Ord a, Fractional a, Show a) => ([a], a) -> IO() -- starting theta, theta0
thetasToString (theta, theta0) = putStr ("Theta: \n " ++ foldr (\x acc -> show x ++ "\n" ++ acc) "\n" theta ++ "theta 0: "++ show theta0 ++ "\n")



x :: (Num a, Ord a, Fractional a) => [[a]]
x = [[15, 3, 1], [4, 18, 1], [-5,-2, 1], [-1, -10, 1], [-5, 15, 1]]
y :: (Num a, Ord a, Fractional a) => [a]
y = [1, -1, -1, 1, -1]
