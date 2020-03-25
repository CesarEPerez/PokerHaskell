module Poker where
    
        --deal function
        deal cards = do
                --original hands
                let orighand1 = [cards!!0,cards!!2,cards!!4,cards!!6,cards!!8]
                let orighand2 = [cards!!1,cards!!3,cards!!5,cards!!7,cards!!9]
                --hands with just values of the cards
                let values1 = (getValues orighand1)
                let values2 = (getValues orighand2)
                --hands with just suits of the cards
                let suits = (getSuits orighand1)
                let suits = (getSuits orighand2)
                --Going to try to put both in an array of tuples like [(1,"C"),(2,"D"),etc]
                print vhand1
                print vhand2

        getValues hand = do
                map getValue hand

        getSuits hand = do
                map getSuit hand

        getValue card =
                (card `mod` 13)

        getSuit card = do
                let x = card/13
                if x <= 1 then "C"
                else if x <= 2 then "D"
                else if x <= 3 then "H"
                else "S"

        