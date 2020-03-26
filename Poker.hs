module Poker where

        import Data.List 

        --deal function
        deal cards = do
                --original hands
                let orighand1 = [cards!!0,cards!!2,cards!!4,cards!!6,cards!!8]
                let orighand2 = [cards!!1,cards!!3,cards!!5,cards!!7,cards!!9]
                
                --hands with just values of the cards
                let values1 = (map getValue orighand1)
                let values2 = (map getValue orighand2)

                --hands with just suits of the cards
                let suits1 = (map getSuit orighand1)
                let suits2 = (map getSuit orighand2)

                --lists of tuples, not used currently 
                let temphand1 = [(values1!!0,suits1!!0),(values1!!1,suits1!!1),(values1!!2,suits1!!2),(values1!!3,suits1!!3),(values1!!4,suits1!!4)]
                let temphand2 = [(values2!!0,suits2!!0),(values2!!1,suits2!!1),(values2!!2,suits2!!2),(values2!!3,suits2!!3),(values2!!4,suits2!!4)]
                
                --sorting the hands
                let hand1 = sort temphand1
                let hand2 = sort temphand2
                let sortedvalues1 = sort values1
                let sortedvalues2 = sort values2
                
                --calls getscore to find score of each hand
                let score1 = getScore sortedvalues1 suits1
                let score2 = getScore sortedvalues2 suits2

                --returns both scores gonna change this function to output winning hand after tiebreaking done
                [score1,score2]
        
        --calls each function to find the score of the hand and returns the score
        getScore values suits = do
                if (checkRoyalFlush values suits) then 1
                else if (checkStraightFlush values suits) then 2
                else if (checkFour values) then 3
                else if (checkFullHouse values) then 4
                else if (checkFlush suits) then 5
                else if (checkStraight values) then 6
                else if (checkThree values) then 7
                else if (checkTwoPair values) then 8
                else if (checkPair values) then 9
                else 10
                
        --returns the value of the card
        getValue card = fromIntegral(card `mod` 13)

        --returns the suit of the card
        getSuit card = do
                let x = (card `div` 13)
                if x <= 1 then "C"
                else if x <= 2 then "D"
                else if x <= 3 then "H"
                else "S"

        checkRoyalFlush values suits = do
                if (checkStraightFlush values suits) && (values!!4 == 13) then True
                else False

        checkStraightFlush values suits = do
                if ((checkFlush suits) && (checkStraight values)) then True
                else False

        checkFour hand = do
                if ((hand!!0 == hand!!1) && (hand!!1 == hand!!2) && (hand!!2 == hand!!3)) then True
                else if ((hand!!1 == hand!!2) && (hand!!2 == hand!!3) && (hand!!3 == hand!!4)) then True
                else False
        
        checkFullHouse hand = do
                if ((hand!!0 == hand!!1) && (hand!!1 == hand!!2) && (hand!!3 == hand!!4)) then True
                else if ((hand!!0 == hand!!1) && (hand!!2 == hand!!3) && (hand!!3 == hand!!4)) then True
                else False

        checkFlush hand = do
                if (all (=="C") hand) || (all (=="D") hand) || (all (=="H") hand) || (all (=="S") hand) then True
                else False

        checkStraight hand = do
                if ((hand!!0 == 1) && (hand!!1 == 10) && (hand!!2 == 11) && (hand!!3 == 12) && (hand!!4 == 13)) then True
                else if ((hand!!0+1==hand!!1) && (hand!!1+1==hand!!2) && (hand!!2+1==hand!!3) && (hand!!3+1==hand!!4))
                        then True
                else False
        
        checkThree hand = do
                if ((hand!!0 == hand!!1) && (hand!!1 == hand!!2)) then True
                else if ((hand!!1 == hand!!2) && (hand!!2 == hand!!3)) then True
                else if ((hand!!2 == hand!!3) && (hand!!3 == hand!!4)) then True
                else False

        checkTwoPair hand = do
                if ((hand!!0 == hand!!1) && (hand!!2 == hand!!3)) then True
                else if ((hand!!0 == hand!!1) && (hand!!3 == hand!!4)) then True
                else if ((hand!!1 == hand!!2) && (hand!!3 == hand!!4)) then True
                else False

        checkPair hand = do
                if (hand!!0 == hand!!1) || (hand!!1 == hand!!2) || (hand!!2 == hand!!3) || (hand!!3 == hand!!4) then True
                else False

                