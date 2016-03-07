
import Control.Applicative   ( (<$>), (<*>) )
import Control.Arrow         ( (&&&) )
import Control.Monad         ( MonadPlus ( mzero ), unless )

import Data.Aeson            ( FromJSON ( parseJSON ), Value ( Object ), 
                               (.:), decode )
import qualified Data.ByteString.Lazy.Char8 as L ( pack, unpack )
import Data.HashMap.Lazy     ( lookup )
import Data.List             ( sortBy, intercalate )
import Data.Maybe            ( catMaybes )
import Data.Text             ( pack )
import qualified Data.Traversable as T     ( sequence )

import Network               ( withSocketsDo )
import Network.HTTP.Conduit  ( simpleHttp )

import System.Directory      ( createDirectory, doesDirectoryExist, doesFileExist )
import System.FilePath.Posix ( pathSeparator )

import Prelude hiding        ( lookup )



-- Each Steam game is identified by an integer id.

type Id = Integer

-- The time a game has been played in minutes.

type PlayTime = Integer

m .@ str = m .: pack str

-- A game consists of an ID and a play time value.
	
data Game = Game { gameId :: Id, playTime :: PlayTime } deriving Show

instance FromJSON Game where

	parseJSON (Object m) = Game <$> m .@ "appid" <*> m .@ "playtime_forever"
	parseJSON _          = mzero

-- User games are represented as the number of games in total and a list of games.
	
data UserGames = UserGames { totalGames :: Integer, -- number of games the user owns
                             gamesList :: [Game]    -- list of game the user owns
						   }
	deriving Show

addGame :: Game -> UserGames -> UserGames
addGame g (UserGames t gs) = UserGames (1 + t) (g : gs)

alienSwarm :: Game
alienSwarm = Game 630 7740

instance FromJSON UserGames where

	parseJSON (Object m) = UserGames <$> m .@ "game_count" <*> m .@ "games"
	parseJSON _          = mzero

-- An achievement consists of a name and a Boolean value whether it has been achieved yet or not.
	
data Achievement = Achievement { achievementLabel :: String, -- name of the achievement
                                 achievementStatus :: Bool   -- achieved yet?
							   }
	deriving Show

instance FromJSON Achievement where

	parseJSON (Object m) = Achievement <$> m .@ "name" <*> (toBool <$> (m .@ "achieved"))
	parseJSON _          = mzero

toBool :: Integer -> Bool
toBool 0 = False
toBool _ = True
    
-- The meta-data of a game consists of a player for which the data is checked,
-- the name of the game and the list of a achivements achieved by the player.

data GameStats = GameStats { playerID :: Id,                -- stats for which player?
                             gameName :: String,            -- name of the game
							 achievements :: [Achievement], -- list of achievements
                             achievable :: Int
						   }
	deriving Show

setAchievable :: Int -> GameStats -> GameStats
setAchievable n (GameStats p g a _) = GameStats p g a n
    
instance FromJSON GameStats where

    parseJSON (Object m) = GameStats <$> (read <$> (m .@ "steamID"))
                                     <*> m .@ "gameName"
                                     <*> achs
                                     <*> (length <$> achs)
        where achs = m .@ "achievements"
    parseJSON _          = mzero

data Response = Response { getStats :: UserGames } deriving Show

instance FromJSON Response where

	parseJSON (Object m) = Response <$> m .@ "response"
	parseJSON _          = mzero

data PlayerStats = PlayerStats { gameStats :: GameStats } deriving Show

instance FromJSON PlayerStats where

	parseJSON (Object m) = PlayerStats <$> m .@ "playerstats"
	parseJSON _          = mzero

data FullAchievement = 
    FullAchievement { achName        :: String, 
                      achDisplayName :: String
                    } deriving Show

instance FromJSON FullAchievement where

    parseJSON (Object m) = FullAchievement <$> m .@ "name" 
                                           <*> m .@ "displayName"
    parseJSON _          = mzero

data GameAchievements = GameAchievements { gameAchievements :: [FullAchievement] }
    deriving Show

instance FromJSON GameAchievements where
    
    parseJSON (Object m) = GameAchievements <$> m .@ "achievements"
    parseJSON _          = mzero

data FullGameStats = FullGameStats { fullGameName         :: String,
                                     fullGameAchievements :: GameAchievements
                                   }
    deriving Show

instance FromJSON FullGameStats where

    parseJSON (Object m) = FullGameStats <$> m .@ "gameName"
                                         <*> m .@ "availableGameStats"
    parseJSON _          = mzero

data FullGame = FullGame { fullGameStats :: FullGameStats }
    deriving Show

instance FromJSON FullGame where

    parseJSON (Object m) = FullGame <$> m .@ "game"
    parseJSON _          = mzero
                                           
achievedPerGame :: GameStats -> Int
achievedPerGame = length . filter achievementStatus . achievements

quotaPerGame :: GameStats -> Double
quotaPerGame gs = achievedPerGame gs * 100 // achievable gs

roundAt :: Int -> Double -> Double
roundAt pos d | pos >= 0  = round (10 ^ pos * d) // 10 ^ pos
              | otherwise = d

infixl 1 //
(//) :: Integral i => i -> i -> Double
x // y = fromIntegral x / fromIntegral y

average :: Fractional n => [n] -> n
average xs = sum xs / fromIntegral (length xs)

toPrequotas :: [GameStats] -> [(Int, Int)]
toPrequotas = map (achievedPerGame &&& achievable) . filter ((> 0) . quotaPerGame)

totalAchieved :: [GameStats] -> Int
totalAchieved = sum . map fst . toPrequotas

totalAchievements :: [GameStats] -> Int
totalAchievements = sum . map achievable

totalAchievements2 :: FullGame -> Int
totalAchievements2 = length . gameAchievements . fullGameAchievements . fullGameStats

totalQuota :: [GameStats] -> Double
totalQuota gs = totalAchieved gs * 100 // totalAchievements gs

-- The first argument is the precision.

quotasWith :: Int -> [GameStats] -> [Double]
quotasWith n = map (roundAt n . uncurry (\x y -> 100 * x // y)) . toPrequotas

(.+) :: Show a => String -> a -> String
str .+ x = str ++ show x

evaluate :: [GameStats] -> String
evaluate gs = 
	unlines [
        "Total achieved                  : " .+ totalAchieved gs,
	    "Total achievable                : " .+ totalAchievements gs,
		"Absolute quota                  : " .+ roundAt 3 (totalQuota gs),
		"Average completion rate (exact) : " .+ roundAt 3 (average qs),
		"Average completion rate (biased): " .+ roundAt 3 (average (quotasWith 0 gs)),
		"",
		"Perfect games                   : " .+ length g100,
		"75% <= x < 100%                 : " .+ length g75,
		"50% <= x < 75%                  : " .+ length g50,
		"25% <= x < 50%                  : " .+ length g25,
		"0%  <= x < 25%                  : " .+ length g0,
		"",
		"Total number of games           : " .+ length qs,
		"Sum of the above                : " .+ sum (map length [g100, g75, g50, g25, g0])
			 ]
    where qs         = quotasWith (-1) gs
          ps         = sortBy (flip compare) qs
          (g100, r1) = span (== 100) ps
          (g75,  r2) = span (>= 75) r1
          (g50, r3)  = span (>= 50) r2
          (g25, g0)  = span (>= 25) r3

pquery = "http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?"
gquery = "http://api.steampowered.com/ISteamUserStats/GetUserStatsForGame/v0002/?appid="

fquery = "http://api.steampowered.com/ISteamUserStats/GetSchemaForGame/v2/?key="
          
-- -- main2 :: String -> String -> IO ()
-- main2 key lab = withSocketsDo $ do
	-- -- putStrLn "Please enter your Steam API Key"
	-- -- key <- getLine
	-- -- putStrLn "Please enter the desired Steam ID"
	-- -- lab <- getLine
    -- let query = "key=" ++ key ++ "&steamid=" ++ lab ++ "&format=json"
    -- res <- simpleHttp (pquery ++ query)
    -- maybe (error "Something went wrong") (collect query) (decode res :: Maybe Response)
 -- where
    -- collect query r = mapM (perGame key lab) (alienSwarm : gamesList (getStats r)) 
                      -- >>= putStrLn . evaluate . catMaybes

-- The directory in which the global game information is stored.
                      
gamesDirectory :: String
gamesDirectory = "games"
                      
-- Check whether a directory exists and create it in the negative case.
                      
checkMakeDir :: String -> IO ()
checkMakeDir dir = do b <- doesDirectoryExist dir
                      unless b (createDirectory dir)
       
checkGamesDirectory :: IO ()
checkGamesDirectory = checkMakeDir gamesDirectory

-- Creates a full path from a folder structure by intercalating the file separator.

dirPath :: [String] -> String
dirPath = intercalate [pathSeparator]

data Refresh = Recent | OnlyAchieved | OnlyAchievable | All deriving Eq

keepGameLocal :: Refresh -> Bool
keepGameLocal Recent       = True
keepGameLocal OnlyAchieved = True
keepGameLocal _            = False

keepUserLocal :: Refresh -> Bool
keepUserLocal OnlyAchievable = True
keepUserLocal _              = False

perGame :: Refresh -> String -> String -> Game -> IO (Maybe GameStats)
perGame refresh key user game = do
    hasGame <- doesFileExist gameFile
    hasUser <- doesFileExist userFile
    let localGame = keepGameLocal refresh && hasGame
        localUser = keepUserLocal refresh && hasUser
    gameStatus <- if localGame
                    then fetch gameFile
                    else simpleHttp gameQuery
    userStatus <- if localUser
                    then fetch userFile
                    else simpleHttp userQuery
    let mstats = do fg <- decode gameStatus
                    user <- decode userStatus
                    return (setAchievable (totalAchievements2 fg) (gameStats user))
    T.sequence (fmap (\_ ->    update localGame gameFile gameStatus
                            >> update localUser userFile userStatus) mstats)
    return mstats
    where 
        gameFile           = dirPath [gamesDirectory, gameLabel]
        userFile           = dirPath [user, gameLabel]
        fetch              = fmap L.pack . readFile
        update b file text = unless b (writeFile file (L.unpack text))
        gameLabel          = show (gameId game)
        gameQuery          = fquery ++ key ++ "&appid=" ++ gameLabel
        userQuery          = fquery ++ gameLabel ++ "&key=" 
                                    ++ key ++ "&steamid=" ++ user
        
    
    -- testQuery <- simpleHttp (fquery ++ key ++ "&appid=" ++ show (gameId game))
    -- case decode testQuery of
        -- Nothing -> return Nothing
        -- Just fg -> fmap ( maybe Nothing 
                                -- (Just . setAchievable (totalAchievements2 fg) 
                                      -- . gameStats )
                                 -- . decode)
                   -- (simpleHttp (gquery ++ show (gameId game) 
                                       -- ++ "&key=" 
                                       -- ++ key
                                       -- ++ "&steamid=" 
                                       -- ++ lab 
                                       -- ++ "&format=json"))