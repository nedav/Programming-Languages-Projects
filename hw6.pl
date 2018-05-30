/*
Resources consulted:
http://www.gprolog.org/manual/html_node/gprolog062.html
http://www.gprolog.org/manual/html_node/gprolog024.html
*/

/* Problem 1 */
/* Base case: True if X and Y both have no elements */
duplist([], []).

/* Recursive step: True if the first element of X and the first two elements of Y are the same and if this is true for the rest of the elements */
duplist([X | T1], [X | [X | T2]]) :- duplist(T1, T2).

/* Problem 2 */
/* Recursive step if first elements of X and Y are the same: True if the rest of elements are also True */
subseq([X | T1], [X | T2]) :- subseq(T1, T2).

/* Recursive step if the first elemnts of X and Y are not the same: True if one of the other elements of Y matches with it */
subseq([X | T1], [_| T2]) :- subseq([X | T1], T2).

/* Base case: True if X is an empty list */
subseq([], _).

/* Problem 3 */
/* Acceptable numbers */
num(0).
num(1).
num(2).
num(3).
num(4).
num(5).
num(6).
num(7).
num(8).
num(9).

/* goodList defines the constraints for a list*/
/* Base case: If the list has one element, it is True if the element is an acceptable number */
goodList([H]) :- num(H).

/* Iterative step: True if all the variables' values are different, if the head is an acceptable number, and if all the other elements are good */
goodList([H|T]) :- goodList(T), num(H), fd_all_different([H|T]).

/* makeNumber makes the digits associated with the letters into a number */
/* Base case: If the list has one element, the result is just the digit in the ones-place */
makeNumber([H], H).

/* Iterative step: Add the rest of the digits to the number, get the length of the list, and add the head digit to the correct 10^n place of the digit so far */
makeNumber([H|T], Result) :- makeNumber(T, TResult), length([H|T], Length), Result is H*(10^(Length - 1)) + TResult.

/* True if there is a good list out of the variables given, where the first letters of the words are not 0, and if, after converting the digits to numbers, 
the addition of the first two words is the third word */
verbalarithmetic(LetterList, [H1|T1], [H2|T2], [H3|T3]):- goodList(LetterList), 
														  H1 #\= 0, H2 #\= 0, H3 #\= 0,
													      makeNumber([H1|T1], Word1), makeNumber([H2|T2], Word2), makeNumber([H3|T3], Word3), 
													      =:=(Word1 + Word2, Word3).

/* Problem 4 */

/* Executing a pickup:
You can only pick up the top block from one of the three stacks if the hand is empty.
This results in the stack without its head and the block in the robot's hand. */
executeAction(world([B|T], Stack2, Stack3, none), pickup(B, stack1), world(T, Stack2, Stack3, B)).
executeAction(world(Stack1, [B|T], Stack3, none), pickup(B, stack2), world(Stack1, T, Stack3, B)).
executeAction(world(Stack1, Stack2, [B|T], none), pickup(B, stack3), world(Stack1, Stack2, T, B)).

/* Executing a put down:
You can only put down a block that you are holding.
This results in a the block becoming the head of the stack. */
executeAction(world(Stack1, Stack2, Stack3, B), putdown(B, stack1), world([B|Stack1], Stack2, Stack3, none)).
executeAction(world(Stack1, Stack2, Stack3, B), putdown(B, stack2), world(Stack1, [B|Stack2], Stack3, none)).
executeAction(world(Stack1, Stack2, Stack3, B), putdown(B, stack3), world(Stack1, Stack2, [B|Stack3], none)).

/* Base case: True if, after no actions, the original world and goal world are the same */
blocksworld(World, [], World).

/* Base case: True if the current action can be executed, all the other actions can be executed, and if the original world is the same as the goal world */
blocksworld(World, [HAction|TAction], GoalWorld) :- executeAction(World, HAction, ResultingWorld), blocksworld(ResultingWorld, TAction, GoalWorld).
