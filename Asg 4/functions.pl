% --- Functions.pl ---

/* Prolog Not */
not( X ) :- X, !, fail.
not( _ ).

/* Converts DMS to Radians */
dms_radian( degmin( Deg, Min ), Result ) :-
    Result is (Deg + Min / 60) * pi / 180.

/* Calculates great circle distance */
haversine_radians( Lat1, Lng1, Lat2, Lng2, Distance ) :-
    dms_radian( Lat1, RLat1 ),
    dms_radian( Lng1, RLng1 ),
    dms_radian( Lat2, RLat2 ),
    dms_radian( Lng2, RLng2 ),
    Dlat is RLat2 - RLat1,
    Dlng is RLng2 - RLng1,
    A is sin( Dlat / 2 ) ** 2
        + cos( RLat1 ) * cos( RLat2 ) * sin( Dlng / 2 ) ** 2,
    Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
    Distance is Dist * 3961.

/* Corrects time if minutes is greater than 60 */
evaluate_time( Hours, Minutes, ArriveHours, ArriveMinutes ) :-
    Minutes >= 60,
    ArriveHours is Hours + 1,
    ArriveMinutes is Minutes - 60.
evaluate_time( Hours, Minutes, ArriveHours, ArriveMinutes ) :-
    Minutes < 60,
    ArriveHours is Hours,
    ArriveMinutes is Minutes.

/* Calculate FlightTime in Hours */
flight_time( Airport1, Airport2, FlightTime ) :-
    airport( Airport1, _, Lat1, Lng1 ),
    airport( Airport2, _, Lat2, Lng2 ),
    haversine_radians( Lat1, Lng1, Lat2, Lng2, Distance),
    FlightTime is Distance / 500.

/* Calculate Arrival Time from Airport1 to Airport2 */
arrival_time( flight(Airport1, Airport2, time( DH,DM )), ArrivalTime) :-
    flight_time( Airport1, Airport2, FlightTime ),
    DepartureTime is DH + DM / 60,
    ArrivalTime is DepartureTime + FlightTime. %Hours

/* Print Time given only Hours */
print_time( HoursMinutes ) :-
    Hours is floor( HoursMinutes * 60 ) // 60,
    Minutes is floor( HoursMinutes * 60 ) mod 60,
    format( '%2d:%2d', [Hours, Minutes]).

/* Self Explanatory */
to_hours( Hours, Minutes, HM ) :-
    HM is Hours + Minutes / 60.

to_minutes( Hours, Minutes ) :-
    Minutes is Hours * 60.

/* Write the first path available */
writepath( [] ).

writepath( [flight(Depart, Arrive, time(DH, DM))|List]) :-
    airport( Depart, DName, _, _),
    airport( Arrive, AName, _, _),
    to_hours( DH, DM, DepartTime ),
    arrival_time( flight(Depart, Arrive, time(DH, DM)), ArrivalTime),
    format( 'depart   %s   %s', [Depart, DName]),
    print_time( DepartTime ), nl,
    format( 'arrive   %s   %s', [Arrive, AName]),
    print_time( ArrivalTime ), nl,
    writepath( List ).

checktime( H1, time( TH1, TM1 )) :-
    to_hours( TH1, TM1, H2 ),
    to_minutes( H1, M1 ),
    to_minutes( H2, M2 ),
    M1 + 29 < M2.

checkarrival( flight( Dep,Arr,DepTime )) :-
   arrival_time( flight( Dep,Arr,DepTime ), ArrivTime ),
   ArrivTime < 24.

/* Generate a list of a path from Node to End */
listpath( Node, End, [flight( Node, Next, NDep)|Outlist] ) :-
    not(Node = End),
    flight(Node, Next, NDep),
    listpath( Next, End, [flight( Node, Next, NDep)], Outlist ).
listpath( Node, Node, _, [] ).
listpath( Node, End,
    [flight( PDep, PArr, PDepTime )| Tried],
    [flight( Node, Next, NDep )| List] ) :-
    flight( Node, Next, NDep),
    arrival_time( flight( PDep, PArr, PDepTime ), PArriv),
    checktime( PArriv, NDep ),
    checkarrival( flight( Node, Next, NDep )),
    Tried2 = append( [flight( PDep, PArr, PDepTime )], Tried ),
    not( member( Next, Tried2 )),
    not( Next = PArr ),
    listpath( Next, End, 
    [flight( Node, Next, NDep )|Tried2], 
    List ).

/* Main Query to be called */
fly( Depart, Arrive ) :-
    listpath( Depart, Arrive, List),
    nl,
    writepath(List), !.

fly( Depart, Depart ) :-
    write( 'Departure and Arrival airports are the same' ),
    !, fail.

