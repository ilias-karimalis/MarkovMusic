

% Synonyms generalize the program 

stomach(S) :- member(S, [stomach, tummy, belly, intestines, intestinal, ventral]).
pain(S) :- member(S, [pain, pains, ache, hurts, hurting, owie]).
anger(S) :- member(S, [anger, aggressiveness, fury, temper, rage, violence]).
preposition(P) :- member(P, [in, inside, within, on, of]).

% Diagnoses are logical combinations of symptoms

diagnosis(Ln, ebola) :- symptoms(Ln, stomach_ache), symptoms(Ln, diarrhea).
diagnosis(Ln, rabies) :- symptoms(Ln, anger), symptoms(Ln, bitten_by_animal).
diagnosis(Ln, cs221) :- symptoms(Ln, butt_pain).+

% Searches the line inputted by user for symptoms

symptoms([S, P|T], stomach_ache):- stomach(S), pain(P).
symptoms([head, P|T], headache) :- pain(P).
symptoms(Ln, diarrhea) :- member(diarrhea, Ln).
symptoms([S|T], anger) :- anger(S).
symptoms(Ln, bitten_by_animal) :- write("y/n have you been bitten by an animal within the last 20 to 60 days?"),
     readln(Newln), member(y, Newln).  

% Recursivly calls the symptoms fn on the rest of the list

symptoms([H|T], S):- symptoms(T, S).


% uppon finding a preposition, this fn reverses the arguments. Ie pain in stomach -> stomach pain

symptoms([H, P, N |T], D) :- preposition(P), symptoms([N, H|T], D).


% Main function

q(Ans) :- 
    write("Enter symptoms: "), flush_output(current_output),
    readln(Ln),
    diagnosis(Ln, Ans).