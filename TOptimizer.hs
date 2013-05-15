--  Author   : Diana Barreto - Ivan Patricio Valarezo
--  Id       : 574386 - 601099
--  Origin   : 06-May-2013
--  Purpose  : Implementation of Optimizer using the Interval Analysis 
--------------------------------------------------------------------------------
-- We are thinking about doing a code optimization that helps eliminating 
-- unreachable code


i=9;
if (i>10){
    j=12;
    k=13;
}else{
    j=0;
    k=0;
}
i=12;

i=9;
j=0;
k=0;

0: EntryNode -> []
1: AsgNode "i" (Con 9) -> [0]
7: AsgNode "j" (Con 0) -> [3]
8: AsgNode "k" (Con 0) -> [7]
9: AsgNode "i" (Con 12) -> [8,6]
10: ExitNode -> [9]

