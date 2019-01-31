module Main where

import System.Environment
import Quest

main :: IO ()
main = do
    args <- getArgs
    parseArgs args


parseArgs :: [String] -> IO()
parseArgs [] = startQuest
parseArgs y@(x:_) = do
    case x of
      "--help" -> usage
      "--version" -> version
      _ -> startQuest


version :: IO ()
version = putStrLn versionStr

versionStr :: String
versionStr = "Шуточная текстовая игра квест: Сдача экзамена по Haskell.\n" ++
             "Version 0.1.0.4\n" ++
             "(c) Тимуркаева Лилия, 2019"

usage :: IO()
usage = putStrLn usageStr

usageStr :: String
usageStr = "Это шуточная текстовая игра квест: Сдача экзамена по Haskell.\n\
             \Для удобства воспользуйтесь картинкой, прикрепленной в репозитории.\n\
             \В начале игры Вы находитесь в аудитории рядом со столом.\n\
             \За столом сидит строгий [Преподаватель] и принимает экзамен по языку Haskell.\n\
             \Вам нужно сдать экзамен. Используйте доступные команды:\n\
             \         И Идти [Направление]\n\
             \Идти в каком-нибудь направление (возможные направления:\n\
             \Вверх, Вниз, Восток, Запад.\n\
             \         В Взять [Объект]\n\
             \Вы можете взять какой-нибудь объект, например, Билет.\n\
             \         С Сдать [Объект] [Объект]\n\
             \Команда для сдачи экзамена Преподавателю.\n\
             \         Б Объединить [Объект] [Объект]\n\
             \Позволяет объединить два объекта.\n\
             \         Г Программировать [Объект] [Объект]\n\
             \Позволяет запрограммировать на Компьютере.\n\
             \         Р Рюкзак\n\
             \Показывает, что есть у Вас в рюкзаке.\n\
             \         О Оглядется\n\
             \Показывает, что вокруг Вас.\n\
             \         Ы Выйти\n\
             \Выход из игры.\n\
             \         П Помощь\n\
             \Показывает список доступных команд.\n\
             \То, с чем можно взаимодействовать в игре, заключено в [] или написано с большой буквы.\n"


