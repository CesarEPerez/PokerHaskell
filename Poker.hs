module Poker where

        import Data.List 

        -- easy dealing; hand1 goes first, followed by hand2, instead of alternating. used for testing.
        easy cards = do
                let deck = [cards!!0,cards!!5,cards!!1,cards!!6,cards!!2,cards!!7,cards!!3,cards!!8,cards!!4,cards!!9]
                deal deck

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

                --list of rank and suit
                let temphand1 = [(values1!!0,suits1!!0),(values1!!1,suits1!!1),(values1!!2,suits1!!2),(values1!!3,suits1!!3),(values1!!4,suits1!!4)]
                let temphand2 = [(values2!!0,suits2!!0),(values2!!1,suits2!!1),(values2!!2,suits2!!2),(values2!!3,suits2!!3),(values2!!4,suits2!!4)]
                
                --sorting the hands
                let hand1 = sort temphand1
                let hand2 = sort temphand2
                let sortedorig1 = sort orighand1
                let sortedorig2 = sort orighand2
                let sortedvalues1 = sort values1
                let sortedvalues2 = sort values2
                let sortedsuits1 = sort suits1
                let sortedsuits2 = sort suits2
                
                --calls getscore to find score of each hand
                let score1 = getScore sortedvalues1 suits1
                let score2 = getScore sortedvalues2 suits2

                -- gets the winning hand and checks tiebreaks, then returns the winning hand as a list of strings
                let winningHand = tieBreak score1 score2 hand1 hand2 sortedsuits1 sortedsuits2 values1 values2 orighand1 orighand2 sortedvalues1 sortedvalues2 sortedorig1 sortedorig2
                let retHand = sort (map tupToStr winningHand)
                retHand
                
        --change tuple to string
        tupToStr tup = do
                let str = show (fst tup) ++ (snd tup)
                str

        --tiebreak function returns winning hand
        tieBreak score1 score2 hand1 hand2 sortedsuits1 sortedsuits2 values1 values2 orighand1 orighand2 sortedvalues1 sortedvalues2 sortedorig1 sortedorig2 = do 
                if score1 == score2 then
                        if score1 == 1 then
                                tieRoyalFlush hand1 hand2 sortedsuits1 sortedsuits2
                        else if score1 == 2 then
                                tieStraightFlush hand1 hand2 values1 values2 orighand1 orighand2
                        else if score1 == 3 then
                                tieFour hand1 hand2 values1 values2 orighand1 orighand2
                        else if score1 == 4 then
                                tieFullHouse hand1 hand2 values1 values2 orighand1 orighand2
                        else if score1 == 5 then
                                tieFlush hand1 hand2 sortedvalues1 sortedvalues2 sortedorig1 sortedorig2
                        else if score1 == 6 then
                                tieStraight hand1 hand2 sortedvalues1 sortedvalues2 sortedorig1 sortedorig2
                        else if score1 == 7 then
                                tieThree hand1 hand2 values1 values2 orighand1 orighand2
                        else if score1 == 8 then
                                tieTwoPair hand1 hand2 values1 values2 orighand1 orighand2
                        else if score1 == 9 then
                                tiePair hand1 hand2 values1 values2 orighand1 orighand2
                        else tieHigh hand1 hand2 sortedvalues1 sortedvalues2 sortedorig1 sortedorig2
                else if score1 < score2 then hand1
                else hand2

        --function to check if hand is a royal flush
        checkRoyalFlush values suits = do
                if (checkStraightFlush values suits) && (values!!4 == 13) then True
                else False

        --function to check if hand is a straight flush
        checkStraightFlush values suits = do
                if ((checkFlush suits) && (checkStraight values)) then True
                else False

        --function to check if hand is a four of a kind
        checkFour hand = do
                if ((hand!!0 == hand!!1) && (hand!!1 == hand!!2) && (hand!!2 == hand!!3)) then True
                else if ((hand!!1 == hand!!2) && (hand!!2 == hand!!3) && (hand!!3 == hand!!4)) then True
                else False
        
        --function to check if hand is a full house
        checkFullHouse hand = do
                if ((hand!!0 == hand!!1) && (hand!!1 == hand!!2) && (hand!!3 == hand!!4)) then True
                else if ((hand!!0 == hand!!1) && (hand!!2 == hand!!3) && (hand!!3 == hand!!4)) then True
                else False

        --function to check if hand is a flush
        checkFlush hand = do
                if ((all (=="C") hand) || (all (=="D") hand) || (all (=="H") hand) || (all (=="S") hand)) then True
                else False

        --function to check if hand is a straight
        checkStraight hand = do
                if ((hand!!0 == 1) && (hand!!1 == 10) && (hand!!2 == 11) && (hand!!3 == 12) && (hand!!4 == 13)) then True
                else if ((hand!!0+1==hand!!1) && (hand!!1+1==hand!!2) && (hand!!2+1==hand!!3) && (hand!!3+1==hand!!4))
                        then True
                else False
        
        --function to check if hand is a three of a kind
        checkThree hand = do
                if ((hand!!0 == hand!!1) && (hand!!1 == hand!!2)) then True
                else if ((hand!!1 == hand!!2) && (hand!!2 == hand!!3)) then True
                else if ((hand!!2 == hand!!3) && (hand!!3 == hand!!4)) then True
                else False

        --function to check if hand is a two pair
        checkTwoPair hand = do
                if ((hand!!0 == hand!!1) && (hand!!2 == hand!!3)) then True
                else if ((hand!!0 == hand!!1) && (hand!!3 == hand!!4)) then True
                else if ((hand!!1 == hand!!2) && (hand!!3 == hand!!4)) then True
                else False

        --function to check if hand is a pair
        checkPair hand = do
                if (hand!!0 == hand!!1) || (hand!!1 == hand!!2) || (hand!!2 == hand!!3) || (hand!!3 == hand!!4) then True
                else False

        -- tie breakers
        -- tie break Royal Flush
        tieRoyalFlush hand1 hand2 suits1 suits2 = do
                if (suits1!!0 > suits2!!0) then hand1
                else hand2
        
        -- tie break Straight Flus
        tieStraightFlush hand1 hand2 values1 values2 orighand1 orighand2 = do
                if maximum values1 > maximum values2 then hand1
                else if maximum values1 < maximum values2 then hand2
                else if maximum orighand1 > maximum orighand2 then hand1
                else hand2

        -- tie break Four Of A Kind
        tieFour hand1 hand2 values1 values2 orighand1 orighand2 = do
                if getFourOfAKindValue values1 > getFourOfAKindValue values2 then hand1
                else if getFourOfAKindValue values1 < getFourOfAKindValue values2 then hand2
                else if getOneOfAKindValue values1 > getOneOfAKindValue values2 then hand1
                else if getOneOfAKindValue values1 < getOneOfAKindValue values2 then hand2
                else if getHighestSuitOfCardRepeating 1 values1 orighand1 > getHighestSuitOfCardRepeating 1 values2 orighand2 then hand1
                else hand2

        -- tie break Full House
        tieFullHouse hand1 hand2 values1 values2 orighand1 orighand2 = do
                if getThreeOfAKindValue values1 > getThreeOfAKindValue values2 then hand1
                else if getThreeOfAKindValue values1 < getThreeOfAKindValue values2 then hand2
                else if getTwoOfAKindValue values1 > getTwoOfAKindValue values2 then hand1
                else if getTwoOfAKindValue values1 < getTwoOfAKindValue values2 then hand2
                else if getHighestSuitOfCardRepeating 2 values1 orighand1 > getHighestSuitOfCardRepeating 2 values2 orighand2 then hand1
                else hand2

        -- tie break Flush
        tieFlush hand1 hand2 sortedvalues1 sortedvalues2 sortedorig1 sortedorig2 = do
                if sortedvalues1!!4 > sortedvalues2!!4 then hand1
                else if sortedvalues1!!4 < sortedvalues2!!4 then hand2
                else if sortedvalues1!!3 > sortedvalues2!!3 then hand1
                else if sortedvalues1!!3 < sortedvalues2!!3 then hand2
                else if sortedvalues1!!2 > sortedvalues2!!2 then hand1
                else if sortedvalues1!!2 < sortedvalues2!!2 then hand2
                else if sortedvalues1!!1 > sortedvalues2!!1 then hand1
                else if sortedvalues1!!1 < sortedvalues2!!1 then hand2
                else if sortedvalues1!!0 > sortedvalues2!!0 then hand1
                else if sortedvalues1!!0 < sortedvalues2!!0 then hand2
                else if sortedorig1!!4 > sortedorig2!!4 then hand1
                else hand2
        
        -- tie break Straight
        tieStraight hand1 hand2 sortedvalues1 sortedvalues2 sortedorig1 sortedorig2 = do
                if sortedvalues1!!4 > sortedvalues2!!4 then hand1
                else if sortedvalues1!!4 < sortedvalues2!!4 then hand2
                else if sortedorig1!!4 > sortedorig2!!4 then hand1
                else hand2

        -- tie break Three Of A Kind
        tieThree hand1 hand2 values1 values2 orighand1 orighand2 = do
                if getThreeOfAKindValue values1 > getThreeOfAKindValue values2 then hand1
                else if getThreeOfAKindValue values1 < getThreeOfAKindValue values2 then hand2
                else do
                        let kickers1 = getKickers values1 2
                        let kickers2 = getKickers values2 2
                        if kickers1!!1 > kickers2!!1 then hand1
                        else if kickers1!!1 < kickers2!!1 then hand2
                        else if kickers1!!0 > kickers2!!0 then hand1
                        else if kickers1!!0 < kickers2!!0 then hand2
                        else if getHighestSuitOfCardRepeating 3 values1 orighand1 > getHighestSuitOfCardRepeating 3 values2 orighand2 then hand1
                        else if getHighestSuitOfCardRepeating 3 values1 orighand1 < getHighestSuitOfCardRepeating 3 values2 orighand2 then hand2
                        else if getHighestSuitOfHighestKicker 2 values1 orighand1 > getHighestSuitOfHighestKicker 2 values2 orighand2 then hand1
                        else hand2

        -- tie break Two Pairs
        tieTwoPair hand1 hand2 values1 values2 orighand1 orighand2 = do
                if getTwoOfAKindValue values1 > getTwoOfAKindValue values2 then hand1
                else if getTwoOfAKindValue values1 < getTwoOfAKindValue values2 then hand2
                else if getSecondTwoOfAKindValue values1 > getSecondTwoOfAKindValue values2 then hand1
                else if getSecondTwoOfAKindValue values1 < getSecondTwoOfAKindValue values2 then hand2
                else if getOneOfAKindValue values1 > getOneOfAKindValue values2 then hand1
                else if getOneOfAKindValue values1 < getOneOfAKindValue values2 then hand2
                else if getHighestSuitOfCardRepeating 2 values1 orighand1 > getHighestSuitOfCardRepeating 2 values2 orighand2 then hand1
                else if getHighestSuitOfCardRepeating 2 values1 orighand1 < getHighestSuitOfCardRepeating 2 values2 orighand2 then hand2
                else if getHighestSuitOfSecondPair values1 orighand1 > getHighestSuitOfSecondPair values2 orighand2 then hand1
                else if getHighestSuitOfSecondPair values1 orighand1 < getHighestSuitOfSecondPair values2 orighand2 then hand2
                else if getHighestSuitOfCardRepeating 1 values1 orighand1 > getHighestSuitOfCardRepeating 1 values2 orighand2 then hand1
                else hand2

        -- tie break One Pair
        tiePair hand1 hand2 values1 values2 orighand1 orighand2 = do
                if getTwoOfAKindValue values1 > getTwoOfAKindValue values2 then hand1
                else if getTwoOfAKindValue values1 < getTwoOfAKindValue values2 then hand2
                else do
                        let kickers1 = getKickers values1 3
                        let kickers2 = getKickers values2 3
                        if kickers1!!2 > kickers2!!2 then hand1
                        else if kickers1!!2 < kickers2!!2 then hand2
                        else if kickers1!!1 > kickers2!!1 then hand1
                        else if kickers1!!1 < kickers2!!1 then hand2
                        else if kickers1!!0 > kickers2!!0 then hand1
                        else if kickers1!!0 < kickers2!!0 then hand2
                        else if getHighestSuitOfCardRepeating 2 values1 orighand1 > getHighestSuitOfCardRepeating 2 values2 orighand2 then hand1
                        else hand2

        -- tie break High Card
        tieHigh hand1 hand2 sortedvalues1 sortedvalues2 sortedorig1 sortedorig2 = do
                if sortedvalues1!!4 > sortedvalues2!!4 then hand1
                else if sortedvalues1!!4 < sortedvalues2!!4 then hand2
                else if sortedvalues1!!3 > sortedvalues2!!3 then hand1
                else if sortedvalues1!!3 < sortedvalues2!!3 then hand2
                else if sortedvalues1!!2 > sortedvalues2!!2 then hand1
                else if sortedvalues1!!2 < sortedvalues2!!2 then hand2
                else if sortedvalues1!!1 > sortedvalues2!!1 then hand1
                else if sortedvalues1!!1 < sortedvalues2!!1 then hand2
                else if sortedvalues1!!0 > sortedvalues2!!0 then hand1
                else if sortedvalues1!!0 < sortedvalues2!!0 then hand2
                else if sortedorig1!!4 > sortedorig2!!4 then hand1
                else hand2

                
        -- wrapper functions
        getFourOfAKindValue hand = findRankRepeating 4 0 hand -- get the rank of a four-card set
        getThreeOfAKindValue hand = findRankRepeating 3 0 hand -- get the rank of a three-card set
        getTwoOfAKindValue hand = findRankRepeating 2 0 hand -- get the rank of the first found pair
        getSecondTwoOfAKindValue hand = findRankRepeating 2 0 (reverse hand) -- get the rank of the second found pair
        getOneOfAKindValue hand = findRankRepeating 1 0 hand -- get the rank of the first found card in 'hand' that appears only once (used for single kickers)
        getHighestSuitOfCardRepeating times values hand = getHighestSuitOfCardRepeating' times 0 values hand 0 -- get the highest suit of a card that repeats 'times' times
        getHighestSuitOfSecondPair values hand = getHighestSuitOfCardRepeating' 2 0 (reverse values) (reverse hand) 0 -- get the highest suit of the second pair found in 'hand'
        getKickers hand amount = getKickers' hand 0 amount [] -- gets 'amount' kickers found in 'hand'
        getHighestSuitOfHighestKicker amount values hand = getHighestSuitOfHighestKicker' amount 0 values hand 0 0 -- get the highest suit of the highest ranked kicker out of 'amount' kickers in 'hand'

        -- helper functions
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
        getValue card = do
                if fromIntegral(card `mod` 13) == 0 then 13
                else fromIntegral(card `mod` 13)

        --returns the suit of the card
        getSuit card = do
                let x = (card `div` 13)
                if x < 1 || card == 13 then "C"
                else if x < 2 || card == 26 then "D"
                else if x < 3 || card == 39 then "H"
                else "S"

        -- credit StackOverflow
        count e [] = 0
        count e (a:xs) = (count e xs +) $ if a == e then 1 else 0

        -- find the rank of a card that repeats 'times' times
        findRankRepeating times index hand = do
                if count (hand!!index) hand == times then hand!!index
                else findRankRepeating times (index+1) hand

        -- get the highest suit of a hard repeating 'times' times
        getHighestSuitOfCardRepeating' times index values hand highest = do
                if index == 5 then highest
                else if count (values!!index) values == times then do
                        let value = hand!!index
                        if value >= 40 then value
                        else if value >= highest then getHighestSuitOfCardRepeating' times (index+1) values hand value
                        else getHighestSuitOfCardRepeating' times (index+1) values hand highest
                else getHighestSuitOfCardRepeating' times (index+1) values hand highest

        -- get the highest suit of the highest ranked kicker out of a total of 'amount' kickers in 'hand'
        getHighestSuitOfHighestKicker' amount index values hand highestRank highestSuit = do
                let rank = values!!index
                let suit = hand!!index
                if index == 5 then highestSuit
                else if count (values!!index) values /= (5-amount) && rank >= highestRank then do
                        if suit >= 40 then suit
                        else if suit > highestSuit then 
                                getHighestSuitOfHighestKicker' amount (index+1) values hand rank suit
                        else getHighestSuitOfHighestKicker' amount (index+1) values hand highestRank highestSuit
                else getHighestSuitOfHighestKicker' amount (index+1) values hand highestRank highestSuit

        -- get a sorted list of 'amount' kickers in 'hand' ('output' is the rank, not the suit)
        getKickers' hand index amount output = do
                if index == 5 then (sort output)
                else if count (hand!!index) hand == (5-amount) then getKickers' hand (index+1) amount output
                else getKickers' hand (index+1) amount (output ++ [(hand!!index)])
