
% offerMean(X, Y) -> Transportation mean Y is used with offer X

offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1), bus). 

% offerAccommodation(X, Y) -> Accommodation Y is part of offer X

offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1), cabin). 

% customerPreferredActivity(X, Y, R) -> Y is the preferred activity kind wrt customer X with relevance R

customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100). 
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100). 
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20). 
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60). 
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20). 
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding, 50). 

% customerPreferredMean(X, Y, R) -> Y is the preferred transportaion mean wrt customer X with relevance R

customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100). 
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10). 

% customerPreferredAccommodation(X, Y, R) -> Y is the preferred accommodation to customer X with relevance R

customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20). 
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50). 
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel, 100). 
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin, 79).


perm([H|T],L) :- perm(T,P), insert(H,P,L).
perm([],[]).
insert(X,L,[X|L]).
insert(X,[H|T],[H|T1]) :- insert(X,T,T1).

possibleSubset([], []).
possibleSubset([E|Tail], L):-
  possibleSubset(Tail, NTail),
  perm([E|NTail],L).
possibleSubset([_|Tail], L):-
  possibleSubset(Tail, NTail),
  perm(NTail,L).


choosePreferences([], []).
choosePreferences([E|Tail], [E|NTail]):-
  E\=activity(_),
  choosePreferences(Tail, NTail).
choosePreferences([E|Tail], NTail):-
  E\=activity(_),
  choosePreferences(Tail, NTail).

choosePreferences1([H], [H]).
choosePreferences1([E|Tail], [E|NTail]):-
  E\=activity(_),
  choosePreferences1(Tail, NTail).
choosePreferences1([E|Tail], NTail):-
  E\=activity(_),
  choosePreferences1(Tail, NTail).
   
choosePreferences([activity(L1)|T],[activity(L2)|T1]):- 
	 choosePreferences1(L1, L2),
	 choosePreferences(T,T1).

choosePreferences([activity(L1)|T],T1):-
	 choosePreferences(T,T1).	 



preferenceSatisfaction(Offer, Customer, [], 0).
preferenceSatisfaction(Offer, Customer, [H|T], S):-
	\+ H =activity(_),
	\+ H = accommodation(_),
	\+ H =means(_),
	preferenceSatisfaction(Offer, Customer, T, S).

preferenceSatisfaction(Offer, Customer, [activity([])|T], S):-
		preferenceSatisfaction(Offer, Customer, T, S).
	
preferenceSatisfaction(offer(_, L1, _, _, _, _, _, _), Customer, [activity([H1|T1])|T], S):-
	\+ member(H1,L1),
	preferenceSatisfaction(offer(_, L1, _, _, _, _, _, _), Customer, [activity(T1)|T], S).
	
preferenceSatisfaction(offer(_, L1, _, _, _, _, _, _), Customer, [activity([H1|T1])|T], S):-
	member(H1,L1),
	customerPreferredActivity(Customer,H1,S1),
	preferenceSatisfaction(offer(_, L1, _, _, _, _, _, _), Customer, [activity(T1)|T], S2),
	S is S1+S2.


preferenceSatisfaction(Offer, Customer, [accommodation(X)|T], S):-
	offerAccommodation(Offer,A),
	\+ A=X,
	preferenceSatisfaction(Offer, Customer, T, S).

preferenceSatisfaction(Offer, Customer, [accommodation(X)|T], S):-
	offerAccommodation(Offer,A),
	 A=X,
	customerPreferredAccommodation(Customer,X,S1),
	preferenceSatisfaction(Offer, Customer, T, S2),
	S is S1+S2.
	

preferenceSatisfaction(Offer, Customer, [means(X)|T], S):-
	offerMean(Offer,A),
	\+ A=X,
	preferenceSatisfaction(Offer, Customer, T, S).
	
preferenceSatisfaction(Offer, Customer, [means(X)|T], S):-
	offerMean(Offer,A),
	 A=X,
	customerPreferredMean(Customer,X,S1),
	preferenceSatisfaction(Offer, Customer, T, S2),
	S is S1+S2.
	

overlapPeriod( period(YY1-MM1-DD1,YY2-MM2-DD2), period(YY3-MM3-DD3, YY4-MM4-DD4)):-
	T1 is YY1*365+MM1*30+DD1,
	T2 is YY2*365+MM2*30+DD2,
	T3 is YY3*365+MM3*30+DD3,
	T4 is YY4*365+MM4*30+DD4,
	T1>=T3,
	T1=<T4.
	
overlapPeriod( period(YY1-MM1-DD1,YY2-MM2-DD2), period(YY3-MM3-DD3, YY4-MM4-DD4)):-
	T1 is YY1*365+MM1*30+DD1,
	T2 is YY2*365+MM2*30+DD2,
	T3 is YY3*365+MM3*30+DD3,
	T4 is YY4*365+MM4*30+DD4,	
	T3>=T1,
	T3=<T2.
	
allmemb([H|T],[H|T1]):-
	allmemb(T,T1).
allmemb(_,[]).
allmemb([H|T],[X|T1]):-
	\+ H=X,
	allmemb(T,[X|T1]).
getOffer([],_).	
getOffer([dest(X)|T], offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests)):-
	offerMean(offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests), _),
	getOffer(T,offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests)).
%offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), bus).	
getOffer([accommodation(A)|T], offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests)):-
	offerAccommodation(offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests), A),
	getOffer(T,offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests)).

getOffer([means(A)|T], offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests)):-
	offerMean(offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests), A),
	getOffer(T,offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests)).

getOffer([budget(B)|T], offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests)):-
	offerMean(offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests), _),
	B>=Budget,
	getOffer(T,offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests)).

getOffer([period(A,B)|T], offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests)):-
	offerMean(offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests), _),
	overlapPeriod(period(A,B),P),
	getOffer(T,offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests)).

getOffer([activity(L)|T], offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests)):-
		offerMean(offer(X,Activities, Budget, Validfrom, Validto, P, Duration, Numberofguests), _),
		allmemb(L1,L),
		getOffer(T,offer(X,L1, Budget, Validfrom, Validto, P, Duration, Numberofguests)).
		
recommendOfferForCustomer(Prefs, [H|T], O):-
	choosePreferences(Prefs, [H|T]),
	getOffer([H|T],O).
%insertion_sort(List,Customers,PreferenceList,Offer,Customers).
insertion_sort(List,Customers,PreferenceList,Offer,Sorted):-i_sort(List,Customers,PreferenceList,Offer,[],Sorted).
i_sort([],_,_,_,X,X).
i_sort([H|T],Customers,PreferenceList,Offer,Accumulator,Sorted):-
	insert1(Offer,H,Customers,PreferenceList,Accumulator,N),
	i_sort(T,Customers,PreferenceList,Offer,N,Sorted).
insert1(_,X,_,_,[],[X]).	
insert1(Offer,X,Customers,PreferenceList,[Y|T],[Y|NT]):-
	getPref(X,Customers,PreferenceList,Z1),
	getPref(Y,Customers,PreferenceList,Z2),
	preferenceSatisfaction(Offer,X,Z1,S1),
	preferenceSatisfaction(Offer,Y,Z2,S2),
	S1=<S2,
	insert1(Offer,X,Customers,PreferenceList,T,NT).
insert1(Offer,X,Customers,PreferenceList,[Y|T],[X,Y|T]):-
	getPref(X,Customers,PreferenceList,Z1),
	getPref(Y,Customers,PreferenceList,Z2),
	preferenceSatisfaction(Offer,X,Z1,S1),
	preferenceSatisfaction(Offer,Y,Z2,S2),
	S1>S2.

getPref(X,[X|T],[Y|T1],Y).
getPref(X,[A|T],[Y|T1],Z):-
	\+ X=A,
	getPref(X,T,T1,Z).

recommendOffer(Customers, PreferenceList, Offer, CustomersChosen):-
	 helper(PreferenceList,Offer),
	 insertion_sort(Customers,Customers,PreferenceList,Offer,Sorted),
	 getN(Offer,N),
	 choose(N,Sorted,CustomersChosen).

getN(offer(_,_,_, _, _, _,_, N),N).

helper([H|T], Offer):-helper( T, Offer).
helper([H|T], Offer):-
	 choosePreferences(H,H1),
	 recommendOfferForCustomer(H1,_,Offer). 		
		
choose(N,[H|T],[]):- \+N>0.
choose(_,[],[]).
choose(N,[H|T],[H|CustomersChosen]):-
	N>0,
	N1 is N-1,
	choose(N1,T,CustomersChosen).
	
	 
 		 
