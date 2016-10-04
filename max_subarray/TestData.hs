import MaxSubarray

testData = [13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7]
testAns = [18, 20, -7, 12]

test :: Bool
test = 
    testAns == (maxSubarray testData)
