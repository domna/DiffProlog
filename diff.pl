/* Benutzte Prädikate für die Mathestrukturen:
	sum(X,Y) - Summe von X und Y
	sub(X,Y) - Subtraktion von X und Y
	pow(X,Y) - X hoch Y
	prod(X,Y) - Produkt von X und Y
	quot(X,Y) - Quotient von X und Y
	v(X) - X ist eine Variable
	c(X) - X ist eine Konstante
	neg(X) - Negativ
	ln(X) - natürlicher Logarithmus
	sin(X) - Sinus von X
	cos(X) - Cosinus von X
*/

diffOP(X,Y,O) :- simplify(X,XS), d(XS,Y,O).

/* Vereinfachung
 ********************/
% Produkte
simp(prod(neg(X),Y),neg(Z)) :- simp(prod(X,Y),Z), !.	% (-x)*y = -x*y
simp(prod(X,neg(Y)),neg(Z)) :- simp(prod(X,Y),Z), !. 	% rev "
simp(prod(c(1),X),Y) :- simp(X,Y), !.			% 1*x = x
simp(prod(X,c(1)),Y) :- simp(X,Y), !.			% rev "
simp(prod(c(0),_),c(0)) :- !.				% 0*_ = 0
simp(prod(_,c(0)),c(0)) :- !.				% rev "
simp(prod(X,X),pow(Y,c(2))) :- simp(X,Y), !.
simp(prod(pow(X,Y),X),pow(XS,sum(YS,c(1)))) :- simp(X,XS), simp(Y,YS), !.
simp(prod(prod(Y,X),X), prod(YS,pow(XS,c(2)))) :- simp(Y,YS), simp(X,XS), !.
simp(prod(prod(Y,pow(X,Z)),X), prod(YS,pow(XS,ZS))) :- simp(X,XS), simp(Y,YS), simp(sum(Z,c(1)),ZS), !.
simp(prod(prod(c(X),Y),c(Z)),E) :- number(X), number(Z), K is X * Z, simp(prod(c(K),Y),E), !.	% 4*x*2 = 8*x
simp(prod(c(X),c(Y)),c(E)) :- number(X), number(Y), E is X * Y, !.				% 4*2 = 8
simp(prod(prod(Y,c(X)),c(Z)),E) :- simp(prod(prod(c(X),Y),c(Z)),E), !.				% x*4*2 = 8*x
simp(prod(c(Z),prod(c(X),Y)),E) :- number(X), number(Z), K is X * Z, simp(prod(c(K),Y),E), !.	% 2*x*4 = 8*x
simp(prod(c(Z),prod(Y,c(X))),E) :- simp(prod(c(Z),prod(c(X),Y)),E), !.				% 4*x*2 = 4*2*x
simp(prod(X,quot(Y,Z)),E) :- simp(quot(prod(X,Y),Z),E), !.					% x * y/z = (x*y)/z

% Summen
simp(sum(X,X),E) :- simp(prod(c(2),X),E), !.	% x+x = 2*x
simp(sum(c(0),X),Y) :- simp(X,Y), !.		% 0+x = x
simp(sum(X,c(0)),Y) :- simp(X,Y), !.		% rev "
simp(sum(X,neg(Y)),Z) :- simp(sub(X,Y),Z), !.	% x+(-y) = x-y
simp(sum(neg(X),Y),Z) :- simp(sub(Y,X),Z), !.	% rev "
simp(sum(neg(X),neg(Y)),neg(Z)) :- simp(sum(X,Y),Z), !.	% -(x)+(-y) = -(x+y)
simp(sum(c(X),c(Y)),c(Z)) :- number(X), number(Y), Z is X + Y, !.	% 4 + 2 = 6
simp(sum(prod(c(X1),Y),prod(c(X2),Y)),E) :- simp(prod(sum(c(X1),c(X2)),Y),E), !.		% 5*x + 4*x = 9*x
simp(sum(prod(Y,c(X1)),prod(Y,c(X2))),E) :- simp(sum(prod(c(X1),Y),prod(c(X2),Y)),E), !.	% rev "
simp(sum(prod(Y,c(X1)),prod(c(X2),Y)),E) :- simp(sum(prod(c(X1),Y),prod(c(X2),Y)),E), !.	% rev "
simp(sum(prod(c(X1),Y),prod(Y,c(X2))),E) :- simp(sum(prod(c(X1),Y),prod(c(X2),Y)),E), !.	% rev "

% Differenzen
simp(sub(c(0),X),Y) :- simp(neg(X),Y), !. 	% 0-x = -x
simp(sub(X,c(0)),Y) :- simp(X,Y), !. 	% rev "
simp(sub(X,X),c(0)) :- !.		% x-x = 0
simp(sub(c(X),c(Y)),c(Z)) :- number(X), number(Y), Z is X - Y, Z >= 0, !. 	% 4 - 2 = 2
simp(sub(c(X),c(Y)),neg(c(Z))) :- number(Y), number(X), Z is Y - X, Z >= 0, !.	% 2 - 4 = -2
simp(sub(X,neg(Y)),sum(X,Y)) :- !.
simp(sub(prod(c(X1),Y),prod(c(X2),Y)),E) :- simp(prod(sub(c(X1),c(X2)),Y),E), !.		% 5*x - 4*x = 9*x
simp(sub(prod(Y,c(X1)),prod(Y,c(X2))),E) :- simp(sub(prod(c(X1),Y),prod(c(X2),Y)),E), !.	% rev "
simp(sub(prod(Y,c(X1)),prod(c(X2),Y)),E) :- simp(sub(prod(c(X1),Y),prod(c(X2),Y)),E), !.	% rev "
simp(sub(prod(c(X1),Y),prod(Y,c(X2))),E) :- simp(sub(prod(c(X1),Y),prod(c(X2),Y)),E), !.	% rev "

% Negativ
simp(neg(neg(X)),Y) :- simp(X,Y), !. % -(-1) = 1

% Hoch
simp(pow(X,c(1)),Y) :- simp(X,Y), !.	% x^1 = x
simp(pow(_,c(0)),c(1)) :- !.		% _^0 = 1
simp(pow(pow(X,c(Y)),c(Z)),E) :- number(Y), number(Z), K is Y * Z, simp(pow(X,c(K)),E), !. % x^4^2 = x^8
simp(pow(c(X),c(Y)),c(E)) :- power(X,Y,E). %number(X), number(Y), E is X^Y, !.	% 4 ^ 2 = 16

% Power Funktion, weil tuProlog 'X is a ^ b' nicht unterstützt
power(B,P,B) :- number(B), number(P), P =:= 1.
power(B,P,E) :- number(B), number(P), PN is P - 1, power(B,PN,EN), E is EN*B.

% Natürlicher Logarithmus von e
simp(ln(c(e)),c(1)) :- !. % ln(e) = 1

% Divisionen
simp(quot(_,c(0)),_) :- !, fail. % 1/0 nicht erlaubt -> Diff nicht möglich
simp(quot(X,X),c(1)) :- !. % x/x = 1
simp(quot(pow(X,c(XP)),pow(X,c(YP))),E) :- number(XP), number(YP), XP >= YP, ZP is XP - YP, simp(pow(X,c(ZP)),E), !.		% x^y/k^z = x^(y-z)
simp(quot(pow(X,c(XP)),pow(X,c(YP))),E) :- number(XP), number(YP), XP < YP, ZP is YP - XP, simp(quot(c(1),pow(X,c(ZP))),E), !.	% rev " mit z > y
simp(quot(c(0),_),c(0)) :- !.						% 0/_ = 0
simp(quot(c(X),c(Y)),c(Z)) :- number(X), number(Y), Z is X / Y, !.	% 4/2 = 2
simp(quot(c(X),prod(c(X),Z)),E) :- simplify(quot(c(1),Z),E), !.		% 4/4*x = 1/x
simp(quot(c(X),prod(Z,c(X))),E) :- simplify(quot(c(1),Z),E), !.		% rev "

simp(X,E) :- X =.. [F,A1,A2], simp(A1,E1), simp(A2,E2), E =.. [F,E1,E2], !. % Spalte Terme auf die keine eigene Vereinfachung besitzen (Um die Unterterme zu vereinfachen)
simp(X,X) :- !. % catchall

% Term aufspalten zum Vereinfachen
simplify(v(X),v(X)) :- !.
simplify(c(X),c(X)) :- !.
simplify(X,Z) :- X =.. [F,A1,A2], simplify(A1,S1), simplify(A2,S2), Y =.. [F,S1,S2], simp(Y,Z), !.
simplify(X,Z) :- X =.. [F,A], simplify(A,S), Y =.. [F,S], simp(Y,Z), !.


/* Differenzierungsregeln
 *************************/
% diff vars & constants
d(v(X),X,c(1)) :- !. % x' -> 1
d(v(_),_,c(0)) :- !. % dx/dy -> 0
d(c(_),_,c(0)) :- !. % const' -> 0

% Diff Regeln für Potenzen
d(pow(c(X),Y),D,Z) :- d(Y,D,Dy), simplify(prod(Dy,prod(ln(c(X)),pow(c(X),Y))),Z), !. 		% (const^y)' -> y' * ln(const) * const^y
d(pow(X,Y),D,Z) :- d(Y,D,c(0)), d(X,D,E), simplify(prod(E,prod(Y,pow(X,sub(Y,c(1))))),Z), !.		% (x^const)' -> const * x' * x^(const-1)
d(pow(X,Y),D,Z) :- d(X,D,c(0)), d(Y,D,Dy), simplify(prod(Dy,prod(ln(X),pow(X,Y))),Z), !.	% (x^y)' falls dx/dD = 0 -> y' * ln(x) * x^y
d(pow(X,neg(c(Y))),D,Z) :- d(quot(c(1),pow(X,c(Y))),D,Z), !.					% (x^(-const))' -> (1/x^const)'

% Diff Regeln für Summen
d(sum(X,Y),D,Z) :- d(X,D,E1), d(Y,D,E2), simplify(sum(E1,E2),Z), !.	% (x+y)' -> x' + y'
d(sub(X,Y),D,Z) :- d(X,D,E1), d(Y,D,E2), simplify(sub(E1,E2),Z), !.	% (x-y)' -> x' - y'

% Diff Regeln für Produkte
d(prod(c(X),Y),D,Z) :- d(Y,D,Dy), simplify(prod(c(X),Dy),Z), !.				% (const*y)' -> const*y' 
d(prod(Y,c(X)), D,Z) :- d(prod(c(X),Y),D,Z), !.						% (y*const)' -> const*y'
d(prod(X,Y),D,Z) :- d(X,D,Dx), d(Y,D,Dy), simplify(sum(prod(Dx,Y),prod(Dy,X)),Z), !.	% (x*y)' -> x'*y + y'*x

% Diff Regeln für negative Terme
d(neg(X),D,Z) :- d(X,D,Dx), simplify(neg(Dx),Z), !.	% (-x)' -> -x'

% Diff Regeln für Quotienten
d(quot(Y,c(X)),D,Z) :- d(Y,D,E), simplify(quot(E,c(X)),Z), !.						% (x/const)' -> x'/const
d(quot(X,Y),D,Z) :- d(X,D,Dx), d(Y,D,Dy), simplify(quot(sub(prod(Dx,Y),prod(Dy,X)),pow(Y,c(2))),Z), !.	% (x/y)' -> (x'*y-y'*x)/y^2

% Diff Regeln für Trigonometrie
d(sin(X),D,Z) :- d(X,D,Dx), simplify(prod(Dx,cos(X)),Z), !.		% sin(x)' -> x' * cos(x)
d(cos(X),D,Z) :- d(X,D,Dx), simplify(prod(Dx,neg(sin(X))),Z), !.	% cos(x)' -> x' * -sin(x)

% Diff Regeln für ln
d(ln(X),D,Z) :- d(X,D,Dx), simplify(quot(Dx,X),Z), !.			% ln(x)' -> x' * 1/x
