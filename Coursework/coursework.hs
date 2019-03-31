--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- Add your student number
--
import Data.Char
import Data.List
import Text.Printf

--
-- Types
--
type Title = String
type Artist = String
type Year = Int
type Sales = Int

-- Define Album type here
type Album = (Title, Artist, Year, Sales)
type Database = [Album]

testData :: [Album]
testData = [("Greatest Hits","Queen", 1981, 6300000),
            ("Gold: Greatest Hits","ABBA", 1992, 5400000),
            ("Sgt. Pepper's Lonely Hearts Club Band","The Beatles", 1967, 5340000),
            ("21", "Adele", 2011, 5110000),
            ("(What's the Story) Morning Glory?", "Oasis", 1995, 4940000),
            ("Thriller", "Michael Jackson", 1982, 4470000),
            ("The Dark Side of the Moon", "Pink Floyd", 1973, 4470000),
            ("Brothers in Arms", "Dire Straits", 1985, 4350000),
            ("Bad", "Michael Jackson", 1987, 4140000),
            ("Rumours", "Fleetwood Mac", 1977, 4090000),
            ("Greatest Hits II", "Queen", 1991, 3990000),
            ("Back to Black", "Amy Winehouse", 2006, 3940000),
            ("The Immaculate Collection", "Madonna", 1990, 3700000),
            ("25", "Adele", 2015, 3500000),
            ("Stars", "Simply Red", 1991, 3450000),
            ("Come On Over", "Shania Twain", 1998, 3430000),
            ("x", "Ed Sheeran", 2014, 3380000),
            ("Legend", "Bob Marley", 1984, 3380000),
            ("Bat Out of Hell", "Meat Loaf", 1977, 3370000),
            ("Back to Bedlam", "James Blunt", 2004, 3360000),
            ("Urban Hymns", "The Verve", 1997, 3340000),
            ("Bridge over Troubled Water", "Simon & Garfunkel", 1970, 3260000),
            ("1", "The Beatles", 2000, 3230000),
            ("Spirit", "Leona Lewis", 2007, 3170000),
            ("Crazy Love", "Michael Bublé", 2009, 3130000),
            ("No Angel", "Dido", 2000, 3090000),
            ("White Ladder", "David Gray", 1998, 3020000),
            ("The Fame", "Lady Gaga", 2009, 2990000),
            ("Only by the Night", "Kings of Leon", 2008, 2980000),
            ("A Rush of Blood to the Head", "Coldplay", 2002, 2960000),
            ("Talk on Corners", "The Corrs", 1997, 2960000),
            ("Spice", "Spice Girls", 1996, 2960000),
            ("Life for Rent", "Dido", 2003, 2900000),
            ("Beautiful World", "Take That", 2006, 2880000),
            ("The Joshua Tree", "U2",  1987, 2880000),
            ("Hopes and Fears", "Keane", 2004, 2860000),
            ("The War of the Worlds", "Jeff Wayne", 1978, 2800000),
            ("X&Y", "Coldplay", 2005, 2790000),
            ("Jagged Little Pill", "Alanis Morissette", 1995, 2780000),
            ("Tubular Bells", "Mike Oldfield", 1973, 2760000),
            ("Scissor Sisters", "Scissor Sisters", 2004, 2760000),
            ("...But Seriously", "Phil Collins", 1989, 2750000),
            ("Tracy Chapman", "Tracy Chapman", 1988, 2710000),
            ("Parachutes", "Coldplay", 2000, 2710000),
            ("The Man Who", "Travis", 1999, 2687500),
            ("Greatest Hits", "ABBA", 1975, 2606000),
            ("I've Been Expecting You", "Robbie Williams", 1998, 2586500),
            ("Come Away with Me", "Norah Jones", 2002, 2556650),
            ("Graceland", "Paul Simon", 1986, 2500000),
            ("Ladies & Gentlemen: The Best of", "George Michael", 1998, 2500000)]

-- ******************
-- helper functions
-- ******************

--displayAlbum :: Album -> String
--displayAlbum (title, artist, year, sales) = "\nTitle: " ++ title ++ "\nArtist: " ++ artist ++ "\nYear: " ++ (show year) ++ "\nSales: " ++ (show sales) ++ "\n"

-- ************************************************************************************************************

displayAlbumString :: Album -> String
displayAlbumString (title, artist, year, sales) = printf "\n%-50s %-30s %-14d %-14d" title artist year sales

-- ************************************************************************************************************

displayList :: [String] -> String
displayList [] = ""
displayList [x] = x ++ "." ++  displayList []
displayList (x:xs) = x ++ ", " ++ displayList xs

--topSales (tt0, at0, yr0, ss0) (tt1, at1, yr1, ss1)
--  | ss0 > ss1 = GT
--  | otherwise = LT

-- ***************************************************

albumYear :: Int -> Int -> Album -> Bool
albumYear minY maxY (t, a, y, s)
  | y <= maxY && y >= minY = True
  | otherwise = False

yearSort (t1, a1, y1, s1) (t2, a2, y2, s2)
  | y1 < y2 = GT
  | otherwise = LT

-- ***************************************************

-- ***************************************************

titleWithTh :: String -> Album -> Bool
titleWithTh title (t, a, y, s)
  | title == t = True
  | otherwise = False

-- ***************************************************

displayQueen :: String -> Album -> Bool
displayQueen artist (t, a, y, s)
  | artist == a = True
  | otherwise = False

--displayArtistFigure (tt0, at0, yr0, ss0) (tt1, at1, yr1, ss1)
--  | ss0 + ss1 = GT
--  | otherwise = LT

-- ***************************************************
-- Main function
-- ****************

-- i. Display all albums
albumsToString :: Database -> String
albumsToString database = concat(map displayAlbumString database)

-- ii. Display top 10 albums in descending order of sales
displayTopTen :: Database -> String
displayTopTen database = albumsToString (take 10 database)

--iii. Display albums released between given years
betweenYears :: Int -> Int -> Database -> Database
betweenYears minYear maxYear database = sortBy yearSort (filter (albumYear minYear maxYear) database)

-- iv. Display all ablums who titles being with a given prefix
displayTitleWithTh :: String -> Database -> String
displayTitleWithTh title database = albumsToString (filter (titleWithTh title) database)

-- v. Display the total sales figures for a given artist
displayArtistName :: String -> Database -> String
displayArtistName a database = albumsToString (filter(displayQueen a) database)


-- vi. Display a list of pairs of artist names withh number of albums they have in top50 (each of them appearing once)


-- vii. Remove 50th album and add new album into the list
addAlbum :: Album -> Database -> Database
addAlbum album database = album : database

-- viii. Increase the sales figure for one of the ablums given its title & artist & sales


--
-- Demo function to test basic functionality (without persistence - i.e.
-- testData doesn't change and nothing is saved/loaded to/from albums file).

demo :: Int -> IO ()
--demo 1 = putStrLn (albumsToString testData)
demo 1 = putStrLn (albumsToString testData)
--demo 2 = putStrLn (albumsToString (top10 testData))
demo 2 = putStrLn (displayTopTen testData)
--demo 3  = putStrLn ( all albums released between 2000 and 2008 inclusive )
demo 3 = putStrLn (albumsToString(betweenYears 2000 2006 testData))
--demo 4 = putStrLn ( all albums with titles beginning with "Th" )
--demo 4 = putStrLn (displayTitleWithTh "Th" testData)
--demo 5  = putStrLn ( total sales figure for "Queen")
demo 5 = putStrLn (displayArtistName "Queen" testData)
--demo 51 =
--demo 6  = putStrLn ( all artists with the number of times they appear in top 50 )

--demo 7  = putStrLn ( albums after removing 50th album and adding "Progress"
--                     by "Take That" from 2010 with 2700000 sales )
demo 7 = putStrLn (albumsToString(addAlbum("Progress", "Take That", 2010, 2700000) testData))
--demo 8  = putStrLn ( albums after increasing sales of "21" by "Adele" by 400000 )

--
--
-- Your user interface (and loading/saving) code goes here
--
--
main :: IO ()
main = do db <- readFile "albums.txt"
          putStrLn "Enter your name: "
          let database = read db :: [Album]
          userName <- getLine
          database <- userInterface (userName, database)
          writeFile "albums.txt" (show database)
          putStrLn "\n\nYour changes to the database has been successfull. :)"
userInterface :: (String, Database) -> IO Database
userInterface (userName, database) = do let info = (userName, database)
                                        let message1 = "Press Enter to go back to the main menu: "
                                        putStrLn "_____________________"
                                        putStrLn "|  Album Database    |"
                                        putStrLn "___________________________________________________________________________________________________________|"
                                        putStrLn "| 1 | Display all albums                                                                                   |"
                                        putStrLn "| 2 | Display top 10 albums                                                                                |"
                                        putStrLn "| 3 | Give all albums that were released between two given years (inclusive)                               |"
                                        putStrLn "| 4 | Give all albums whose titles begin with a given prefix                                               |"
                                        putStrLn "| 5 | Give the total sales figure for a given artist                                                       |"
                                        putStrLn "| 6 | Give a list of pairs of artists’ names with the number of albums they have in the top 50             |"
                                        putStrLn "| 7 | Remove the 50th (lowest-selling) album and add a given (new) album into the list                     |"
                                        putStrLn "| 8 | Increase the sales figure for one of the albums given its title & artist and the additionalsales     |"
                                        putStrLn "| 0 |Exit and update database                                                                              |"
                                        putStrLn "|__________________________________________________________________________________________________________|"
                                        putStr   "|Select opetion 0 to 8: "
                                        input <- getLine
                                        if input /= "0"
                                           then case input of
                                                     "1" -> do info <- selection 1 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "2" -> do info <- selection 2 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "3" -> do info <- selection 3 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     --"4" -> do info <- selection 4 info
                                                    --           entry <- getLine
                                                    --           userInterface info
                                                    -- "5" -> do info <- selection 5 info
                                                    --           entry <- getLine
                                                    --           userInterface info
                                                  --   "6" -> do info <- selection 6 info
                                                  --             entry <- getLine
                                                --               userInterface info
                                                --     "7" -> do info <- selection 7 info
                                              --                 entry <- getLine
                                                --               userInterface info
                                                --     "8" -> do info <- selection 8 info
                                                --               entry <- getLine
                                                --               userInterface info
                                                     _ -> do putStrLn "======================== You have entered an invalid number. =========================="
                                                             userInterface info
                                        else return (snd info)
--selection :: Int -> (String, Database) -> IO (String, Database)

--selection 1 (userName, database) = do putStrLn "Display Albums"
--		                                  putStrLn (albumsToString database)
--                                    return (userName, database)
selection 2 (userName, database) = do putStrLn "================== Display top 10 sales =================="
                                      putStrLn (displayTopTen database)
                                      return (userName, database)
selection 3 (userName, database) = do putStrLn "=============== Give all albums that were released between two given years (inclusive) ======================="
                                      putStrLn "=================== Enter Start Year ===================="
                                      year <- getLine
                                      let firstYear = read year :: Int
                                      putStrLn "==================== Enter End Year ====================="
                                      year <- getLine
                                      let lastYear = read year :: Int
                                      putStrLn  (albumsToString(betweenYears firstYear lastYear database))
                                      return (userName, database)
--selection 4 (userName, database) = do putStrLn "============================ Give all albums whose titles begin with a given prefix ==============================="
--                                      putStrLn ("Enter title")
  --                                    title <- getLine
  --                                    putStrLn (displayTitleWithTh title database)
  --                                    return (userName, database)
--selection 5 (userName, database) = do putStrLn "============================ Give the total sales figure for a given artist ======================================="
--				      putStrLn "Enter artist name "
--				      artist <- getLine
--				      putStrLn ("Total sales for artist " ++ artist ++ " : " ++  displayArtist artist database)
--                                      return (userName, database)
