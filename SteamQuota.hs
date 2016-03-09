module SteamQuota where

import Control.Applicative             ( (<$>), (<*>) )
import Control.Arrow                   ( (&&&) )
import Control.Monad                   ( MonadPlus ( mzero ), unless )
import Control.Monad.Trans.Class       ( lift )
import Control.Monad.Trans.Maybe       ( MaybeT, runMaybeT )

import Data.Aeson                      ( FromJSON ( parseJSON ), Value ( Object ), (.:), decode, Object )
import Data.Aeson.Types                ( Parser )
import Data.ByteString.Lazy.Char8      ( pack, unpack, writeFile, readFile, ByteString )
import Data.Char                       ( toLower )
import Data.HashMap.Lazy               ( lookup )
import Data.List                       ( sortBy, intercalate )
import Data.Maybe                      ( catMaybes )
import qualified Data.Text as Text     ( pack )
import qualified Data.Traversable as T ( sequence )

import GHC.IO.Encoding                 ( setLocaleEncoding, utf8 )

import Network                         ( withSocketsDo )
import Network.HTTP.Conduit            ( simpleHttp )

import System.Directory                ( createDirectory, doesDirectoryExist, doesFileExist )
import System.Environment              ( getArgs )
import System.FilePath.Posix           ( pathSeparator )

import Prelude hiding                  ( lookup, writeFile, readFile )
import qualified Prelude as P          ( readFile )

-- Each Steam game is identified by an integer id.

type Id = Integer

-- The time a game has been played in minutes.

type PlayTime = Integer

-- Shorthand for accessing fields in a JSON object.

(.@) :: FromJSON a => Object -> String -> Parser a
m .@ str = m .: Text.pack str

-- A game consists of an ID and a play time value.
    
data Game = Game { gameId :: Id, playTime :: PlayTime } deriving Show

instance Eq Game where

    Game i _ == Game j _ = i == j

-- Alien Swarm is a free game, whose game stats are bugged: due to the fact that it is free,
-- apparently no user owns it, which is why it is added by hand

alienSwarm :: Game
alienSwarm = Game 630 100

instance FromJSON Game where

    parseJSON (Object m) = Game <$> m .@ "appid" <*> m .@ "playtime_forever"
    parseJSON _          = mzero

-- User games are represented as the number of games in total and a list of games.
    
data UserGames = UserGames { totalGames :: Integer, -- number of games the user owns
                             gamesList :: [Game]    -- list of games the user owns
                           }
    deriving Show

instance FromJSON UserGames where

    parseJSON (Object m) = UserGames <$> (m .@ "response" >>= (.@ "game_count"))
                                     <*> (m .@ "response" >>= (.@ "games"))
    parseJSON _          = mzero

-- An achievement consists of a name and a Boolean value whether it has been achieved yet or not.
    
data Achievement = Achievement { achievementLabel :: String, -- name of the achievement
                                 achievementStatus :: Bool   -- achieved yet?
                               }
    deriving Show

instance FromJSON Achievement where

    parseJSON (Object m) = Achievement <$> m .@ "name" <*> (toBool <$> (m .@ "achieved"))
    parseJSON _          = mzero

-- Convert integers to Booleans.

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

-- Set the number of possible achievements to the supplied value.

setAchievable :: Int -> GameStats -> GameStats
setAchievable n (GameStats p g a _) = GameStats p g a n
    
instance FromJSON GameStats where

    parseJSON (Object m) = GameStats <$> (m .@ "playerstats" >>= (read <$>) . (.@ "steamID"))
                                     <*> (m .@ "playerstats" >>= (.@ "gameName"))
                                     <*> achs
                                     <*> (length <$> achs)
        where achs = m .@ "playerstats" >>= (.@ "achievements")
    parseJSON _          = mzero
    
data RecentGames = RecentGames { recentGames :: [Game] } deriving Show

instance FromJSON RecentGames where

    parseJSON (Object m) = RecentGames <$> (m .@ "response" >>= (.@ "games"))
    parseJSON _          = mzero

data FullAchievement = FullAchievement { achName        :: String, 
                                         achDisplayName :: String
                                       }
    deriving Show

instance FromJSON FullAchievement where

    parseJSON (Object m) = FullAchievement <$> m .@ "name" 
                                           <*> m .@ "displayName"
    parseJSON _          = mzero

data FullGameStats = FullGameStats { fullGameName         :: String,
                                     fullGameAchievements :: [FullAchievement]
                                   }
    deriving Show

instance FromJSON FullGameStats where

    parseJSON (Object m) = FullGameStats <$> (m .@ "game" >>= (.@ "gameName"))
                                         <*> (m .@ "game" >>= (.@ "availableGameStats") 
                                                          >>= (.@ "achievements"))
    parseJSON _          = mzero

-- Computes how many achievements have been achieved per game.
                                 
achievedPerGame :: GameStats -> Int
achievedPerGame = length . filter achievementStatus . achievements

-- Computes the quota (achieved/achievable) per game (double precision).

quotaPerGame :: GameStats -> Double
quotaPerGame gs = achievedPerGame gs * 100 // achievable gs

-- Round a Double value after the supplied number of positions right of the decimal point.
-- If the supplied position is less than zero, the original value is returned.

roundAt :: Int -> Double -> Double
roundAt pos d | pos >= 0  = round (10 ^ pos * d) // 10 ^ pos
              | otherwise = d

-- Shorthand for dividing integer-like numbers and obtaining Doubles.

infixl 1 //
(//) :: Integral i => i -> i -> Double
x // y = fromIntegral x / fromIntegral y

-- Computes the average of a list of fractional values.

average :: Fractional n => [n] -> n
average = uncurry (/) . foldr (\x (s, l) -> (x + s, 1 + l)) (0, 0)

-- Computes the numbers of achieved and achievable achievements per game in a list.
-- A game counts towars the total value only if at least one achievement has been unlocked.

toPrequotas :: [GameStats] -> [(Int, Int)]
toPrequotas = map (achievedPerGame &&& achievable) . filter ((> 0) . quotaPerGame)

totalAchieved :: [GameStats] -> Int
totalAchieved = sum . map fst . toPrequotas

totalAchievements :: [GameStats] -> Int
totalAchievements = sum . map achievable

totalAchievements2 :: FullGameStats -> Int
totalAchievements2 = length . fullGameAchievements

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
        "Total achieved                   : " .+ totalAchieved gs,
        "Total achievable                 : " .+ totalAchievements gs,
        "Absolute quota                   : " .+ roundAt 3 (totalQuota gs),
        "Average completion rate (exact)  : " .+ roundAt 3 (average qs),
        "Average completion rate (biased) : " .+ roundAt 3 (average (quotasWith 0 gs)),
        "",
        "Perfect games                    : " .+ length g100,
        "75% <= x < 100%                  : " .+ length g75,
        "50% <= x < 75%                   : " .+ length g50,
        "25% <= x < 50%                   : " .+ length g25,
        "0%  <= x < 25%                   : " .+ length g0,
        "",
        "(Played) games with achievements : " .+ length qs,
        "Sum of the above                 : " .+ sum (map length [g100, g75, g50, g25, g0])
             ]
    where qs         = quotasWith (-1) gs
          ps         = sortBy (flip compare) qs
          (g100, r1) = span (== 100) ps
          (g75,  r2) = span (>= 75) r1
          (g50, r3)  = span (>= 50) r2
          (g25, g0)  = span (>= 25) r3

{- These queries denote the prefixes of the Steam API pages that need to be queried to obtain
   certain information:
   ownedQuery: for querying the games a user owns
   userStatsQuery: for querying individual game stats per user per game
   schemaQuery: for querying game information with respect to achievements
   recentQuery: querying the list of recently played games of a user.
-}

ownedQuery, userStatsQuery, schemaQuery, recentQuery :: String
ownedQuery = "http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?"
userStatsQuery = "http://api.steampowered.com/ISteamUserStats/GetUserStatsForGame/v0002/?appid="
schemaQuery = "http://api.steampowered.com/ISteamUserStats/GetSchemaForGame/v2/?key="
recentQuery = "http://api.steampowered.com/IPlayerService/GetRecentlyPlayedGames/v0001/?"

-- Request a non-empty string until such a string has been provided.

request :: String -> String -> IO String
request rq text = do
    if null text 
        then putStrLn rq
             >> getLine 
             >>= request rq
        else return text

-- Request a key/user respectively, until some non-empty information has been supplied.

requestKey, requestUser :: String -> IO String
requestKey  = request keyRequest
requestUser = request userRequest

-- Request strings for the requests of keys, user ids, and refresh rates.

keyRequest, userRequest, refreshRate :: String
keyRequest  = "Please enter your Steam API key or save it to the file " ++ show keyFile ++ "."
userRequest = "Please enter a Steam user id or save the default id to " ++ show userFile ++ "."
refreshRate = intercalate "\n" [
                       "Please enter a refresh rate:",
                       "r(ecent): update stats on recently played games (up to ten) only",
                       "g(ame stats): update game status only",
                       "u(nlocked): update unlocked achievements only",
                       "a(ll): update all information (may take a minute)"
                      ]

-- Requests a Steam API key and a user id from the user, until some valid information is obtained.

obtainKeyUser :: [String] -> IO (String, String)
obtainKeyUser (key : user : _) = return (key, user)
obtainKeyUser _                = do
    hasKey <- doesFileExist keyFile
    hasUser <- doesFileExist userFile
    key <- if hasKey then P.readFile keyFile >>= requestKey
                     else requestKey ""
    user <- if hasUser then P.readFile userFile >>= requestUser
                       else requestUser ""
    return (key, user)
        
-- Map words beginning with the right letters to refresh rates.

refreshFromString :: String -> Refresh
refreshFromString ('r' : _) = Recent
refreshFromString ('g' : _) = OnlyAchievable
refreshFromString ('u' : _) = OnlyAchieved
refreshFromString _         = All

-- The directory in which the global game information is stored.
                      
gamesDirectory :: String
gamesDirectory = "games"
                   
-- File that possibly contains the Steam API key.

keyFile :: String
keyFile = "keyfile"

-- File that possibly contains the default user.

userFile :: String
userFile = "defaultuser"

-- Check whether a directory exists and create it in the negative case.
                      
checkMakeDir :: String -> IO ()
checkMakeDir dir = do b <- doesDirectoryExist dir
                      unless b (createDirectory dir)

-- Check whether the game directory exists or not and create it in the negative case.
       
checkGamesDirectory :: IO ()
checkGamesDirectory = checkMakeDir gamesDirectory

-- Creates a full path from a folder structure by intercalating the file separator.

dirPath :: [String] -> String
dirPath = intercalate [pathSeparator]

-- A data type that represents, what information needs to be updated.

data Refresh = Recent | OnlyAchieved | OnlyAchievable | All deriving Eq

-- When is the local information sufficient with respect to games?

keepGameLocal :: Refresh -> Bool
keepGameLocal Recent       = True
keepGameLocal OnlyAchieved = True
keepGameLocal _            = False

-- When is the local information sufficient with respect to users?

keepUserLocal :: Refresh -> Bool
keepUserLocal OnlyAchievable = True
keepUserLocal _              = False

-- Monadic if-then-else construct that either reads from the file supplied in the second
-- argument (if the first one is True) or performs an external query using the query
-- string in the third argument.

readOrFetch :: Bool -> String -> String -> IO ByteString
readOrFetch b local fetch | b         = readFile local
                          | otherwise = simpleHttp fetch

-- Action per game.
-- This function checks whether any local data concerning the game or user exists.
-- If it exists and only local data is required, the local data is read.
-- If it does not exist or an update is required, the corresponding data is fetched
-- and saved for later use.

perGame :: String    -- ^ Steam API key
        -> String    -- ^ Steam user id
        -> Bool      -- ^ whether to read game stats locally (if possible)
        -> Bool      -- ^ whether to read user stats locally (if possible)
        -> Game      -- ^ game in question
        -> MaybeT IO GameStats
perGame key user gameLocal userLocal game = do
    hasGame <- lift (doesFileExist gameFile)
    hasUser <- lift (doesFileExist userFile)
    let localGame = gameLocal && hasGame
        localUser = userLocal && hasUser
    gameStatus <- lift (readOrFetch localGame gameFile gameQuery)
    lift (update localGame gameFile gameStatus)
    case decode gameStatus :: Maybe FullGameStats of
        Nothing -> mzero
        Just fg -> do 
            userStatus <- lift (readOrFetch localUser userFile userQuery)
            lift (update localUser userFile userStatus)
            maybe mzero (return . setAchievable (totalAchievements2 fg)) (decode userStatus)
    where 
        gameFile      = dirPath [gamesDirectory, gameLabel]
        userFile      = dirPath [user, gameLabel]
        update b file = unless b . writeFile file 
        gameLabel     = show (gameId game)
        gameQuery     = schemaQuery ++ key ++ "&appid=" ++ gameLabel ++ "&format=json"
        userQuery     = userStatsQuery ++ gameLabel ++ "&key=" 
                                       ++ key ++ "&steamid=" ++ user ++ "&format=json"

main :: IO ()
main = withSocketsDo $ do
    setLocaleEncoding utf8
    args <- getArgs
    (key, user) <- obtainKeyUser args
    putStrLn refreshRate
    resp <- getLine
    let refresh     = refreshFromString (map toLower resp)
        querySuffix = "key=" ++ key ++ "&steamid=" ++ user ++ "&format=json"
    checkGamesDirectory
    checkMakeDir user
    owned <- simpleHttp (ownedQuery ++ querySuffix)
    recent <- simpleHttp (recentQuery ++ querySuffix)
    let gamesOwned  = fmap ((alienSwarm :) . gamesList) (decode owned) :: Maybe [Game]
        gamesRecent = fmap recentGames (decode recent) :: Maybe [Game]
        updateUser g = maybe (keepUserLocal refresh) 
                             (\rs ->    (g `notElem` rs || keepUserLocal refresh)
                                     && (g `elem` rs || refresh == Recent || refresh == OnlyAchieved))
                             gamesRecent
    case gamesOwned of
        Just os -> mapM (\g -> runMaybeT (perGame key 
                                                  user 
                                                  (keepGameLocal refresh) 
                                                  (updateUser g) 
                                                 g)) os
                              >>= putStrLn . evaluate . catMaybes
        _                  -> putStrLn ("Error while parsing: " ++ unpack owned ++ unpack recent)