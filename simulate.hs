
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay, forkIO)
import System.Random (RandomGen, randomR, mkStdGen)



simPoisson :: RandomGen g => Float -> g -> IO ()
simPoisson lambda g = 
      do 
        (t, h) <- sampleExp lambda g 
        threadDelay $ floor $ t * 1000000
        putChar '*'
        hFlush stdout
        simPoisson lambda h 

backspacePoisson :: RandomGen g => Float -> g -> IO ()
backspacePoisson lambda g =
      do 
        (t, h) <- sampleExp lambda g 
        threadDelay $ floor $ t * 1000000
        (n, j) <- sampleBinomial (Binomial 10 0.1) h
        delete n
        hFlush stdout
        backspacePoisson lambda j 

delete :: Int  -> IO ()
delete n = putStr $ concat $ replicate n "\b \b"   

simulate :: IO () -> IO () -> IO () 
simulate ps1 ps2 = do
  forkIO ps1
  forkIO ps2
  return ()



exponentialInverseCDF :: Float -> Float -> Float
exponentialInverseCDF lambda _ | lambda <= 0 = error "Exponential must have positive parameter"
exponentialInverseCDF lambda y = - log (1 - y) / lambda

sampleExp :: RandomGen g => Float -> g -> IO (Float, g)
sampleExp lambda g = return (exponentialInverseCDF lambda y, h) 
                            where (y,h) = randomR (0::Float, 1::Float) g  


data Binomial = Binomial Int Float

sampleBinomial :: RandomGen g => Binomial -> g -> (Int, g) 
sampleBinomial (Binomial 1 p) g = sampleBernoulli (Bernoulli p) g
sampleBinomial (Binomial n p) g = (x + y, j)
          where (x, h) = sampleBernoulli (Bernoulli p) g
                (y, j) = sampleBinomial (Binomial (n - 1) p) h
          

data Bernoulli = Bernoulli Float

sampleBernoulli :: RandomGen g => Bernoulli -> g -> (Int, g)
sampleBernoulli (Bernoulli p) g    
           | x < p   = (1, h)
           | otherwise    = (0, h)
             where (x,h) = randomR (0::Float, 1::Float) g



-- class RandomVariable a where
--   sample :: RandomGen g => a -> g -> (b,g)

-- instance RandomVariable Bernoulli where
--  sample = sampleBernoulli

-- instance RandomVariable Binomial where
--  sample = sampleBinomial

-- data Exponential = .....

-- instance RandomVariable Exponential where