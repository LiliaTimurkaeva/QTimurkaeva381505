-- "Волшебное заклинание" для использования Deriving
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Еще одно "колдунство" без него проблемы с AMP (Functor-Applicative-Monad Proposal) 
{-# LANGUAGE FlexibleContexts #-}

module Quest(
  startQuest
)where


import Control.Monad (mapM_)
import Control.Monad.State (get, gets, StateT(..), evalStateT, liftIO, put, MonadState(..), MonadIO(..))
-- Добавлено для совместимости с версией 7.10
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
-- Конец добавления
import Data.Char (toUpper, toLower)
import Data.List (delete)
import Text.Printf (printf)
import System.IO (hFlush, stdout)

startQuest :: IO () 
startQuest = do 
    look auditorium 
    won <- evalStateT (runGameState run) initialMap
    case won of
      Won -> putStrLn winString 
      Lost -> putStrLn loseString
      QuitGame -> putStrLn "До свидания!"

-- Объекты в игре
data Object = Шпаргалка | Билет | Практика | Листинг | Преподаватель | Компьютер 
    deriving (Eq, Show, Read)

-- Направление движения игрока
data Direction = Запад | Восток | Вверх | Вниз
    deriving (Eq, Show, Read)

-- Места в игре 
data Room = Терминалкласс | Парта | Аудитория | Врюкзаке 
    deriving (Eq, Show, Read)

-- Пути входа
data Entryway = Дверь | Ступеньки 
    deriving (Eq)
instance Show Entryway where
    show Дверь = "дверь"
    show Ступеньки = "ступеньки"

-- Путь от одного места к другому, и вход
data Path = Path {
    dir :: Direction,
    entryway :: Entryway,
    to :: Room
} deriving (Eq, Show)

-- местоположение: имя, описание, список путей к другим местоположениям и объекты в этом местоположении
data Location = Location {
    name :: Room,
    desc :: String,
    paths :: [Path],
    objects :: [Object]
} deriving (Eq)

-- экземпляр show по умолчанию для местоположения показывающий описание
instance Show Location where
    show = show . desc

-- Пользовательские команды
data Command = Идти Direction |
               И Direction |   
               Взять Object |
               В Object |
               Сдать Object Object |
               С  Object Object |
               Рюкзак |
               Р |
               Оглядется |
               О |
               Программировать Object Object | 
               Г Object Object |
               Объединить Object Object |
               Б Object Object |
               Выйти |
               Ы |
               Помощь |
               П
    deriving (Eq, Show, Read)

-- различные результаты, которые могут иметь действие
data Result = Won | Lost | Continue | QuitGame
    deriving (Eq)

-- тип действия (Идти, Взять, и т.д.)
type GameAction = Object -> Object -> GameState Result

-- определение типа данных для хранения информации об игре
data GS = GS { worldMap :: [Location], currentLocation :: Location, preparedResponse :: Bool, programmReady :: Bool } 
    deriving (Show)

-- and a state transformer monad to automatically thread the state for us
-- Добавлено для совместимости с версией 7.10
instance Functor GameState where
    fmap = liftM

instance Applicative GameState where
    pure = return
    (<*>) = ap
-- Конец добавления

newtype GameState a = GameState { runGameState :: StateT GS IO a }
    deriving (Monad, MonadIO, MonadState GS)

-- Объекты которые можно взять
pickupable = flip elem [Шпаргалка, Билет, Практика, Листинг]

-- Местонахождения
auditorium = Location { name = Аудитория,
    desc = "Вы находитесь в аудитории рядом со столом.\n\
            \За столом сидит строгий [Преподаватель] и принимает экзамен по языку Haskell.\n\
            \На столе у преподавателя лежат билеты и отобранные шпаргалки.",
    paths = [Path Восток Дверь Терминалкласс, Path Вверх Ступеньки Парта],
    objects = [Шпаргалка, Билет, Преподаватель]}
computerroom = Location { name = Терминалкласс,
    desc = "Вы находитесь в терминал классе. Перед вами [Компьютер].\n\
            \Здесь Вы можете получить свое практическое задание\n\
            \и после подготовки в аудитории запрограммировать и отладить его.",
    paths = [Path Запад Дверь Аудитория],
    objects = [Практика, Листинг, Компьютер]}
schooldesk = Location { name = Парта,
    desc = "Вы находитесь в аудитории за партой, где Вы можете подготовиться\n\
            \к сдаче экзамена - объединить билет и ответ на практическое задание.",
    paths = [Path Вниз Ступеньки Аудитория],
    objects = []}
backpack = Location { name = Врюкзаке,
    desc = "",
    paths = [],
    objects = []}

-- сообщения о прохождении или не прохождении квеста
winString  = "Преподаватель проверяет ответ на билет и практику\n\
              \видит, что у Вас с собой листинг программы,\n\
              \поздравляет Вас с успешной сдачей экзамена по языку Haskell\n\
              \и расписывается в зачетке! Квест закончен."
loseString = "Преподаватель проверяет ответ на билет и практику\n\
              \видит что у Вас нет с собой листинга программы.\n\
              \Он расстроен и предлагает Вам прийти c листингом и пересдать экзамен!  Квест закончен."

-- Создаем игровой Мир
initialMap :: GS
initialMap = GS { worldMap = [auditorium, computerroom, schooldesk, backpack], 
    currentLocation = auditorium, 
    preparedResponse = False, 
    programmReady = False }

-- Описание пути
describePath :: Path -> String
describePath p = printf "Есть %s ведущая на %s от сюда." (show $ entryway p) (show . dir $ p)

-- Печатаем описание местоположения
describeLocation :: Location -> IO ()
describeLocation = putStrLn . desc

-- Печатаем все пути, ведущие из текущего местоположения
describePaths :: Location -> IO ()
describePaths = mapM_ (putStrLn . describePath) . paths

-- Печатаем описания объектов в текущем местоположении
describeFloor :: Location -> IO ()
describeFloor = mapM_ putStrLn . map (printf "Вы видите %s." . show) . filter pickupable . objects

-- Печатаем информацию о местоположении, пути и объекте
look :: Location -> IO ()
look loc = do
  describeLocation loc
  describePaths loc
  describeFloor loc

-- есть ли путь, ведущий *d* от *loc* ?
viableDir :: Direction -> Location -> Bool
viableDir d = or . map ((== d) . dir) . paths

-- для объекта location возвращает имя 
getLoc :: Room -> [Location] -> Location
getLoc x = head . filter ((== x) . name)

-- перевод в заглавные
capitalize :: String -> String
capitalize word = (toUpper . head) word : (map toLower (tail word))

-- версия Read
maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                [(x,"")] -> Just x
                _ -> Nothing

-- проверка правильности введенной пользователем команды
parseCommand :: String -> Maybe Command
parseCommand input = maybeRead (caps input) >>= return
    where caps = unwords . map capitalize . words

-- ввод
io = liftIO
write = io . putStrLn

-- объект находится в указанном месте ?
isAt :: Object -> Location -> Bool
isAt obj = elem obj . objects

-- есть ли объект в рюкзаке ?
haveObject :: Object -> GameState Bool
haveObject obj = gets worldMap >>= \w ->
                 let inv = (getLoc Врюкзаке w) in
                 return $ isAt obj inv

-- текущее положение тоже самое ?
currentRoomIs :: Room -> GameState Bool
currentRoomIs room = gets currentLocation >>= return . ((== room) . name)

continue :: (Monad m) => m Result
continue = return Continue

-- студент передвигается
walk :: Direction -> GameState Result
walk di = ableToWalk di >>= \able ->
  if able
    then newLocation di >>= setLocation >> (write $ printf "Идти %s " (show di)) >> continue
    else write "Вы не можете идти по этому пути" >> continue
  where ableToWalk d = gets currentLocation >>= \l -> return (viableDir d l)
        newLocation d = get >>= \t -> return ((flip getLoc (worldMap t)) . to . head $ filter (( == d) . dir) (paths (currentLocation t)))
        setLocation l = get >>= \t -> put t{ currentLocation = l }

-- студент берет в руки объект
pickup :: Object -> GameState Result
pickup obj = ableToPickup obj >>= \able ->
             if able
               then storeObject obj >> (write $ printf "Вы взяли %s" (show obj)) >> continue 
               else write "Вы не можете взять этот предмет." >> continue
  where ableToPickup obj = get >>= \t -> return $ isAt obj (currentLocation t) && pickupable obj
        storeObject obj = get >>= \t ->
                          let c = currentLocation t
                              w = worldMap t
                              newl = c{ objects = (delete obj (objects c)) }
                              inv = getLoc Врюкзаке w
                              newInv = inv { objects = obj : (objects inv) } in
                          put t{ currentLocation = newl, worldMap = (newl : newInv : (delete inv (delete c w))) }

-- действия игры (такие как гулять, сдавать, и.т.д.)

-- have = список объектов, которые должен иметь игрок
-- room = комната где находится игрок
-- obj1, obj2 = объекты, введенные пользователем
-- spec1, spec2 = объекты, необходимые для выполнения действия
-- effect = эффект, который необходимо выполнить в случае успеха действия
-- misc = любые другие разные элементы, которые должны быть в наличии для успеха (листинг)
-- string1,string2 = строка успеха (включая два %s для имен объектов) и строка сбоя соответственно

gameAction :: [Object] -> Room -> Object -> Object -> Object -> Object -> 
              GameState () -> [GameState Bool] -> String -> String -> GameState Bool
gameAction have room obj1 obj2 spec1 spec2 effect misc string1 string2 = do
  haveAll <- return . and =<< mapM haveObject have
  inRoom <- currentRoomIs room
  allMisc <- return . and =<< if null misc then return [True] else sequence misc
  let correctObjects = (obj1 == spec1 && obj2 == spec2)
      result = and [haveAll, inRoom, correctObjects, allMisc]
  if result then effect >> write (printf string1 (show obj1) (show obj2)) >> return True else write string2 >> return False

-- Подготовится и написать ответ на билет и практическое задание
preparesolutions :: GameAction
preparesolutions obj1 obj2 = gameAction [Билет, Практика] Парта Билет Практика obj1 obj2
                 (get >>= \t -> put t{ preparedResponse = True }) []
                 "Вы объединили %s и %s" 
                 "Вы не можете объединить!" >> return Continue
-- Программировать
programming :: GameAction
programming obj1 obj2 = gameAction [Билет, Практика] Терминалкласс Практика Компьютер obj1 obj2 
                 (get >>= \t -> put t{ programmReady = True }) [gets preparedResponse] 
                 "Вы запрограммировали на языке Haskell %s на %s" "Вы не можете запрограммировать!" >> return Continue
-- сдача  
passing:: GameAction
passing obj1 obj2 = gameAction [Практика] Аудитория Практика Преподаватель obj1 obj2 
                   (return ()) [gets preparedResponse, gets programmReady]
                   "Вы отдали %s на проверку %s" "Вы не можете сдать!" >>= \result ->
                   if result then haveObject Листинг >>= return . (?) Won Lost
                             else return Continue

(?) :: a -> a -> Bool -> a
(?) true false test = if test then true else false

help :: GameState Result
help = mapM_ write 
       ["", "  Доступные команды:",
        "   И Идти [Направление]",
        "   В Взять [Объект]",
        "   С Сдать [Объект] [Объект]",
        "   Б Объединить [Объект] [Объект]",
        "   Г Программировать [Объект] [Объект]",
        "   Р Рюкзак",
        "   О Оглядется",
        "   Ы Выйти",
        "   П Помощь", ""] >> continue


getInventory :: GameState [Object]
getInventory = return . objects . getLoc Врюкзаке =<< gets worldMap

-- запускаем игру
run :: GameState Result
run =  do
  t <- get
  -- чтение команды от пользователя
  io . putStr $ "> "
  io . hFlush $ stdout
  line <- io getLine
  result <- case parseCommand line of
    Nothing -> write "Неверная команда!" >> continue
    Just cmd -> do
         case cmd of
           Идти dir -> walk dir
           И dir -> walk dir
           Взять o -> pickup o
           В o -> pickup o
           Сдать o1 o2 -> passing o1 o2
           С o1 o2 -> passing o1 o2
           Объединить o1 o2 -> preparesolutions o1 o2
           Б o1 o2 -> preparesolutions o1 o2
           Программировать o1 o2 -> programming o1 o2
           Г o1 o2 -> programming o1 o2
           Рюкзак -> getInventory >>= io . print >> continue
           Р -> getInventory >>= io . print >> continue
           Оглядется -> io (look (currentLocation t)) >> continue
           О -> io (look (currentLocation t)) >> continue
           Выйти -> return QuitGame
           Ы -> return QuitGame
           Помощь -> help
           П -> help 
  case result of
    Continue -> run
    x -> return x