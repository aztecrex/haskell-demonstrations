module Robot (
    SubSystem (..)
  , throttle
  , steer
  , direction
  , status
  , fire
) where


data SubSystem = Drive | Logic | Weapons | Shields

throttle :: Double -> IO ()
throttle = error "we don't really have a robot"

steer :: Double -> IO ()
steer = error "we don't really have a robot"

direction ::  IO Double
direction = error "we don't really have a robot"

status :: SubSystem -> IO Bool
status = error "we don't really have a robot"

fire :: IO ()
fire = error "we don't really have a robot"


