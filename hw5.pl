
units(X) :- 
    X is 3;
    X is 4.
    
fc_course(Course) :- course(Course, _, X), units(X).

search(C, [A|B]) :- %search for C in list
    A == C;
    search(C, B).


prereq_110(Pre) :- 
    course(Pre, X, _),
    search(ecs110, X). %search
    
ecs140a_students(Students) :-
    student(Students, X),
    search(ecs140a, X).
    
neg(X) :- X, !, fail.
neg(_).

find([H|T], X) :-
    (	search(H, X)
    ->  true
    ;   find(T, X)
    ).
    
instructor_names(Instructor) :-
    instructor(Instructor, X),
    student(john, Y),
    find(X, Y).

students(STUDENTS) :- %students in jim class
    instructor(jim, X),
    student(STUDENTS, Y),
    find(X, Y).
    
allprereq([], []).

allprereq([X|Xs], All_Pre) :-
    allprereq(X, A),
    allprereq(Xs, B),
    append(A, B, All_Pre).

allprereq(Course, []) :-
    neg(course(Course, _, _)).

allprereq(Course , All_Pre) :- 
    course(Course, X, _),
    allprereq(X, Pre),
    append(Pre, X, All_Pre),!.
    
mylength([A|As], B) :- mylength(A, X), all_length(As, Y), B is X + Y.
mylength(A, 1) :- atom(A).
all_length([], 0).
all_length([A|As], X) :-
    mylength(A, B),
    all_length(As, C),
    X is B + C. 

get(X, X, 1). 
get(A, B, 0) :- A \= B.
getA([], 0).
getA([X|Xs], A) :- get(X, a, B), getA(Xs, C), A is B + C. 
getB([], 0).
getB([X|Xs], A) :- get(X, b, B), getB(Xs, C), A is B + C.
equal_a_b(L) :-
    getA(L, A),
    getB(L, B),
    A = B.

%this checks for sublist in the entire list
suffix(X, Z, A) :- append(X, A, Z).
prefix(Z, Y, B) :- append(B, Z, Y).
sublist(X, Y, Prefix, Suffix) :- 
    prefix(Z, Y, Prefix),
    suffix(X, Z, Suffix).
    
swap_prefix_suffix(K, L, S) :-
    sublist(K, L, Prefix, Suffix),
    append(Suffix, K, A),
    append(A, Prefix, S).

myequal([],[]).
myequal([X|Xs], [X|Ys]) :- myequal(Xs, Ys).
myreverse([], []).
myreverse([X|Xs], A) :- myreverse(Xs, B), append(B, [X], A).
palin(A) :-
    myreverse(A, B),
    myequal(A, B).

dec(A, B) :- B is A - 1. %decrement 
seq(List, 0, List). %if counter is zero return list
seq([X|Xs], Counter, A) :- 
    ( 	X is 0  
    ->  dec(Counter, Next), seq(Xs, Next, A)
    ;   seq(Xs, 2, A), dec(Counter, Next), seq(A, Next, _)
    ).
    
good([]).
good([X|Xs]) :- 
    (   X is 1
    ->  seq(Xs, 2, _)
    ;   good(Xs)
    ).

opposite(A, B) :- A = left, B = right.
opposite(A, B) :- A = right, B = left.
unsafe(state(A, B, B, _)) :- opposite(A, B). %wolf and goat
unsafe(state(A, _, B, B)) :- opposite(A, B). %goat and cabbage
safe(state(A, B, C, D)) :- neg(unsafe(state(A, B, C, D))). %safe is not unsafe
state(A, B, C, D) :- safe(state(A, B, C, D)).

arc(take(none, A, B), state(A, X, Y, Z), state(B, X, Y, Z)) :- opposite(A, B).
arc(take(wolf, A, B), state(A, A, Y, Z), state(B, B, Y, Z)) :- opposite(A, B).
arc(take(goat, A, B), state(A, Y, A, Z), state(B, Y, B, Z)) :- opposite(A, B).
arc(take(cabbage, A, B), state(A, Y, Z, A), state(B, Y, Z, B)) :- opposite(A, B).

go(X, X, _).
go(X, Y, Last) :-
    %write(X),write(Y),nl,
    arc(Print, X, Next),
    Next \= Last,
    safe(Next),
    write(Print), nl,
    go(Next, Y, X).
    
solve :-
    go(state(left, left, left, left), state(right, right, right, right),state(left, left, left, left)),
    !.
   