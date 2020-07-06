offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),
bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),
10, 1), cabin).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding,
50).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,
100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,
79).
subset1([],[]).
subset1([H|T],[H|T1]):-
    subset1(T,T1).
subset1([_|T],R):-
    subset1(T,R).
perm([H|T],L) :- perm(T,P), insert(H,P,L).
perm([],[]).
%either insert first
insert(X,L,[X|L]).
%or insert later in the tail
insert(X,[H|T],[H|T1]) :- insert(X,T,T1).
possibleSubset(L,R):-
    subset1(L,R1),
    perm(R1,R).
subsetHelper([],[]).
subsetHelper([PrefH|PrefT],[PrefH|ReultPrefT]):-
    PrefH\=activity(_),
    subsetHelper(PrefT,ReultPrefT).
subsetHelper([activity(A)|T],[H|T1]):-
    subset1(A,AR),
    H=activity(AR),
    subsetHelper(T,T1).

subsetHelper([_|T],R):-
    subsetHelper(T,R).

choosePreferences(Pref,ChosenPref):-
    subsetHelper(Pref,ChosenPref).

preferenceSatisfaction(Offer, Customer,[H|T], S):-
    H=accommodation(A),
    customerPreferredAccommodation(Customer,A,S1),
    preferenceSatisfaction(Offer, Customer,T, S2),
    S is S1+S2.
preferenceSatisfaction(Offer, Customer,[H|T], S):-
    H=means(M),
    customerPreferredMean(Customer,M,S1),
    preferenceSatisfaction(Offer, Customer,T, S2),
    S is S1+S2.

preferenceSatisfaction(Offer,Customer,[H|T],S):-
    Offer=offer(_,OfferedActivities,_,_,_,_,_,_),
    H=activity(AC),
    subset1(OfferedActivities,AC),
    %H byd5ol activity([diving])
    prefActivity(AC,Customer,S1),
    preferenceSatisfaction(Offer,Customer,T, S2),
    S is S1+S2.
preferenceSatisfaction(Offer, Customer,[_|T], S):-
     preferenceSatisfaction(Offer, Customer,T, S).
preferenceSatisfaction(_,_,[],0).
prefActivity([],_,0).
prefActivity([H|T],Customer,S):-
    customerPreferredActivity(Customer,H,S1),
    prefActivity(T,Customer,S2),
    S is S1+S2.
later(Y1-M1-D1,Y2-M2-D2):-
    Y2>Y1;
    Y2=Y1,M2>M1;
    Y2=Y1,M2=M1,D2>D1.
overlapPeriod(period(_-_-_,YE1-ME1-DE1),period(YS2-MS2-DS2,_-_-_)):-
	later(YS2-MS2-DS2,YE1-ME1-DE1).
%getOffer([],false).
% dest(Target)
getOfferHelper(dest(Target), Offer) :-
	offerMean(Offer, _),
	Offer = offer(Target, _, _, _, _, _, _, _).
% activity(Target)
getOfferHelper(activity(ActivityList), Offer) :-
	offerMean(Offer, _),
	Offer = offer(_, Target, _, _, _, _, _, _),
	subset1(Target, ActivityList).
% budget(Budget)
getOfferHelper(budget(Budget), Offer) :-
	offerMean(Offer, _),
	Offer = offer(_, _, Target, _, _, _, _, _),
	Target =< Budget.
% ValidFrom
getOfferHelper(ValidFrom, Offer) :-
	offerMean(Offer, _),
	Offer = offer(_, _, _, ValidFrom, _, _, _, _).
% ValidTo
getOfferHelper(ValidTo, Offer) :-
	offerMean(Offer, _),
	Offer = offer(_, _, _, _, ValidTo, _, _, _).
% period(Target)
% overlapPeriod(period(YS1-MS1-DS1,YE1-ME1-DE1),period(YS2-MS2-DS2,YE2-ME2-DE2))
getOfferHelper(period(YS2-MS2-DS2, YE2-ME2-DE2), Offer) :-
	offerMean(Offer, _),
	Offer = offer(_, _, _, _, _, period(YS1-MS1-DS1, YE1-ME1-DE1), _, _),
	overlapPeriod(period(YS2-MS2-DS2, YE2-ME2-DE2), period(YS1-MS1-DS1, YE1-ME1-DE1)).

% Duration
getOfferHelper(Duration, Offer) :-
	offerMean(Offer, _),
	Offer = offer(_, _, _, _, _, _, Duration, _).
% NumberOfGuests
getOfferHelper(NumberOfGuests, Offer) :-
	offerMean(Offer, _),
	Offer = offer(_, _, _, _, _, _, _, NumberOfGuests).
% Mean
getOfferHelper(mean(Mean), Offer) :-
	offerMean(Offer, Mean).
% Accomodation
getOfferHelper(accommodation(Accom), Offer) :-
	offerAccommodation(Offer, Accom).

getOffer([Head | Tail], Offer) :-
	getOfferHelper(Head, Offer),
	getOffer(Tail, Offer).
getOffer([], _).

recommendOfferForCustomer([], _, _).
recommendOfferForCustomer([], [], _).
recommendOfferForCustomer(Prefs, ChosenPrefs, O) :-
	choosePreferences(Prefs, ChosenPrefs),
	getOffer(ChosenPrefs, O).

max([],0).
max([H],H).
max([H1,H2|T],Max):-
    H1>=H2,
    max([H1|T] , Max).
max([H1,H2|T],Max):-
    H1<H2,
    max([H2|T] , Max).

getAllSatisfactions([],[],_,[]).
getAllSatisfactions([Cust1|CustT],[Pref1|PrefT],Offer,[S|NewS]):-
    recommendOfferForCustomer(Pref1,_,Offer),
    preferenceSatisfaction(Offer,Cust1,Pref1,S),
    getAllSatisfactions(CustT,PrefT,_,NewS).
sortingCustomers([],[],_).
sortingCustomers(S,Customers,[Customer1|CustomersT]):-
     max(S,MaxSatisfaction),
     nth0(Index,S,MaxSatisfaction,UpdatedS),
     nth0(Index,Customers,Customer1,UpdatedCustomers),
     sortingCustomers(UpdatedS,UpdatedCustomers,CustomersT).
insertingInChosenCustomers(_,[],[]).
insertingInChosenCustomers(Offer,[CustomersSortedH|CustomersSortedT],[CustomersSortedH|CutomersChosen]):-
       Offer=offer(_, _, _, _, _, _, _, NoOfGuests),
       length([CustomersSortedH|CutomersChosen],Len),
       Len<NoOfGuests,
       insertingInChosenCustomers(Offer,CustomersSortedT,CutomersChosen).
insertingInChosenCustomers(Offer,[CustomersSortedH|_],[CustomersSortedH|CutomersChosen]):-
       Offer=offer(_, _, _, _, _, _, _, NoOfGuests),
       length([CustomersSortedH|CutomersChosen],Len),
       Len>=NoOfGuests.

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Utilities for reocommed offer %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Create a list from element
insert2(Head,[],[Head]).

insert2(poc(Customer, S), [poc(Customer2, S2)| T],[poc(Customer,S), poc(Customer2,S2)|T]) :-
	S2 =< S.
insert2(poc(Customer, S), [poc(Customer2, S2)| T],[poc(Customer2, S2)| Rest]) :-
    S2 > S,
    insert2(poc(Customer,S),T, Rest).

sorted([], S, S).
sorted([Head | RestOfCustomers], S, Act):-
    insert2(Head, Act, Rest),
    sorted(RestOfCustomers, S, Rest).

sortCustomers([],[]).
sortCustomers(CustomerS, S):-
    sorted(CustomerS, S, []).

getCustomers(_, ChosenCustomers, 0, ChosenCustomers).
getCustomers([],ChosenCustomers, _, ChosenCustomers).
getCustomers([poc(Customer,_)| Rest], ChosenCustomers , N, S):-
    N > 0,
    LessN is N -1,
    getCustomers(Rest, ChosenCustomers, LessN,[Customer | S]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Utilities for reocommed offer %%%%%%%%%%%%%%%%%%%%%%%%%%%%

recommendOffer([],_,_,[]).
% Generic Case
recommendOffer([Customer| RestOfCustomers],[Prefs | RestOfPrefs], Offer, ChosenCustomers):-
    choosePreferences(Prefs,ChosenPrefs),
    getOffer(ChosenPrefs,Offer),
    preferenceSatisfaction(Offer,Customer,ChosenPrefs,S),
    recommendOffer(RestOfCustomers,RestOfPrefs, Offer, ChosenCustomers,[poc(Customer,S)]).

recommendOffer([Customer | RestOfCustomers], [Prefs | RestOfPrefs], Offer, CustomersChosen,CSF):-
    choosePreferences(Prefs,ChosenPrefs),
    getOffer(ChosenPrefs,Offer),
    preferenceSatisfaction(Offer,Customer,ChosenPrefs,S),
    recommendOffer(RestOfCustomers, RestOfPrefs, Offer, CustomersChosen, [poc(Customer,S)|CSF]).

recommendOffer([],_,Offer, ChosenCustomers,CSF):-
    offer(_,_,_,_,_,_,_,N) = Offer,
    sortCustomers(CSF,SCSF),
    getCustomers(SCSF,ChosenCustomers,N,[]).
getAllActivities(L):-
setof(X,Y^Z^customerPreferredActivity(Y,X,Z),L).
insert3(X,[],[X]).
insert3(X,[H|T],[X,H|T]).
mostPreferredActivityH(Customer,[_|T],R):-
    customerPreferredActivity(Customer,_,S),
    insert(S,T,R).


mostPreferredActivity(Customer,Activity):-
    mostPreferredActivityH(Customer,[],R2),
    sort(0,@>=,R2,R),

    R=[H|_],
    customerPreferredActivity(Customer,Activity,H).


