import fpinscala.datastructures.List as LList

LList.hasSubsequence(LList(1,2,3,4), LList(1,2,3))
LList.hasSubsequence(LList(1,2,3,4), LList(2))
LList.hasSubsequence(LList(1,2,3,4), LList(2,3))
LList.hasSubsequence(LList(1,2,3,4), LList(3,4))
LList.hasSubsequence(LList(1,2,3,4), LList(4,3))
